use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::BufReader;

use itertools::Itertools;
use thiserror::Error;

use crate::minizinc_data_parser::DataFile;
use crate::minizinc_data_parser::DznError;
use crate::minizinc_data_parser::MissingValueError;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Precedence {
    pub(crate) predecessor: usize,
    pub(crate) gap: i32,
    pub(crate) successor: usize,
}

pub(crate) struct RcpspInstance {
    pub(crate) resource_capacities: Vec<u32>,
    pub(crate) processing_times: Vec<u32>,
    pub(crate) resource_requirements: Vec<Vec<u32>>,
    pub(crate) dependencies: HashMap<usize, Vec<Precedence>>,
}

impl RcpspInstance {
    fn new(
        resource_capacities: Vec<u32>,
        processing_times: Vec<u32>,
        resource_requirements: Vec<Vec<u32>>,
        precedence_relations: Vec<Precedence>,
    ) -> Self {
        Self {
            resource_capacities,
            processing_times,
            resource_requirements,
            dependencies: precedence_relations
                .into_iter()
                .into_group_map_by(|rel| rel.successor),
        }
    }
}

pub(crate) fn parse_rcpsp_dzn(instance_file: File) -> SchedulingResult<RcpspInstance> {
    let reader: BufReader<File> = BufReader::new(instance_file);
    let dzn_vars = DataFile::parse_dzn(reader)?;
    let number_of_resources = dzn_vars.get_int("n_res")? as u32;
    let number_of_activities = dzn_vars.get_int("n_tasks")? as u32;
    let duration = dzn_vars
        .get_1d_int_array("d")?
        .iter()
        .map(|x| *x as u32)
        .collect::<Vec<_>>();
    let precedences = dzn_vars
        .get_1d_set_of_int_array("suc")?
        .iter()
        .enumerate()
        .flat_map(|(successor, xs)| {
            xs.iter()
                .map(|&x| (x - 1) as usize)
                .map(|predecessor| Precedence {
                    predecessor,
                    gap: duration[predecessor] as i32,
                    successor,
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let instance = RcpspInstance::new(
        dzn_vars
            .get_1d_int_array("rc")?
            .iter()
            .map(|x| *x as u32)
            .collect::<Vec<_>>(),
        duration,
        dzn_vars
            .get_2d_int_array("rr")?
            .iter()
            .map(|xs| xs.iter().map(|x| *x as u32).collect::<Vec<_>>())
            .collect::<Vec<_>>(),
        precedences,
    );
    if number_of_resources != instance.resource_capacities.len() as u32 {
        Err(SchedulingError::InconsistentValues(
            "n_res".to_owned(),
            number_of_resources.into(),
            "length of \"rc\"".to_owned(),
            instance.resource_capacities.len() as i64,
        ))
    } else if number_of_activities != instance.processing_times.len() as u32 {
        Err(SchedulingError::InconsistentValues(
            "n_tasks".to_owned(),
            number_of_activities.into(),
            "length of \"d\"".to_owned(),
            instance.processing_times.len() as i64,
        ))
    } else {
        Ok(instance)
    }
}

pub(crate) fn parse_rcpsp_max_dzn(instance_file: File) -> SchedulingResult<RcpspInstance> {
    let reader: BufReader<File> = BufReader::new(instance_file);
    let dzn_vars = DataFile::parse_dzn(reader)?;
    let number_of_resources = dzn_vars.get_int("n_res")? as u32;
    let number_of_activities = dzn_vars.get_int("n_tasks")? as u32;
    let number_of_precedences = dzn_vars.get_int("n_dc")? as u32;
    let instance = RcpspInstance::new(
        dzn_vars
            .get_1d_int_array("rcap")?
            .iter()
            .map(|x| *x as u32)
            .collect::<Vec<_>>(),
        dzn_vars
            .get_1d_int_array("dur")?
            .iter()
            .map(|x| *x as u32)
            .collect::<Vec<_>>(),
        dzn_vars
            .get_2d_int_array("rr")?
            .iter()
            .map(|xs| xs.iter().map(|x| *x as u32).collect::<Vec<_>>())
            .collect::<Vec<_>>(),
        dzn_vars
            .get_2d_int_array("dcons")?
            .iter()
            .map(|row| Precedence {
                predecessor: (row[0] - 1) as usize,
                gap: row[1],
                successor: (row[2] - 1) as usize,
            })
            .collect::<Vec<_>>(),
    );
    if number_of_resources != instance.resource_capacities.len() as u32 {
        Err(SchedulingError::InconsistentValues(
            "n_res".to_owned(),
            number_of_resources.into(),
            "length of \"rcap\"".to_owned(),
            instance.resource_capacities.len() as i64,
        ))
    } else if number_of_activities != instance.processing_times.len() as u32 {
        Err(SchedulingError::InconsistentValues(
            "n_tasks".to_owned(),
            number_of_activities.into(),
            "length of \"d\"".to_owned(),
            instance.processing_times.len() as i64,
        ))
    } else if number_of_precedences
        != instance
            .dependencies
            .values()
            .map(|dependencies| dependencies.len() as u32)
            .sum()
    {
        Err(SchedulingError::InconsistentValues(
            "n_dc".to_owned(),
            number_of_precedences.into(),
            "length of \"dcons\"".to_owned(),
            instance
                .dependencies
                .values()
                .map(|dependencies| dependencies.len() as i64)
                .sum(),
        ))
    } else {
        Ok(instance)
    }
}

pub(crate) type SchedulingResult<T> = Result<T, SchedulingError>;

#[derive(Error, Debug)]
pub(crate) enum SchedulingError {
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Failed to parse the input file, details: {0}")]
    MalformedFile(#[from] DznError),
    #[error("Failed to extract a key from the input file, details: {0}")]
    DznMissingValue(#[from] MissingValueError),
    #[error("The file {0} is not supported.")]
    InvalidInstanceFile(String),
    #[error("Inconsistency between `{0}` (value {1}) and `{2}` (value {3})")]
    InconsistentValues(String, i64, String, i64),
}

impl SchedulingError {
    pub(crate) fn invalid_instance(path: impl Display) -> Self {
        Self::InvalidInstanceFile(format!("{}", path))
    }
}
