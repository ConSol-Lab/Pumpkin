use std::fmt::Display;
use std::fs::File;
use std::io::BufReader;

use thiserror::Error;

use crate::minizinc_data_parser::DataFile;
use crate::minizinc_data_parser::DznError;
use crate::minizinc_data_parser::MissingValueError;

pub(crate) struct RcpspInstance {
    pub(crate) resource_capacities: Vec<u32>,
    pub(crate) processing_times: Vec<u32>,
    pub(crate) resource_requirements: Vec<Vec<u32>>,
    pub(crate) dependencies: Vec<Vec<usize>>,
}

impl RcpspInstance {
    fn new(
        resource_capacities: Vec<u32>,
        processing_times: Vec<u32>,
        resource_requirements: Vec<Vec<u32>>,
        precedence_relations: Vec<Vec<usize>>,
    ) -> Self {
        let mut dependencies = vec![vec![]; precedence_relations.len()];
        for (current_id, precedence_relation) in precedence_relations.iter().enumerate() {
            for successor_id in precedence_relation.iter() {
                dependencies[*successor_id - 1].push(current_id);
            }
        }

        Self {
            resource_capacities,
            processing_times,
            resource_requirements,
            dependencies,
        }
    }
}

pub(crate) fn parse_rcpsp_dzn(instance_file: File) -> SchedulingResult<RcpspInstance> {
    let reader: BufReader<File> = BufReader::new(instance_file);
    let dzn_vars = DataFile::parse_dzn(reader)?;
    let number_of_resources = dzn_vars.get_int("n_res")? as u32;
    let number_of_activities = dzn_vars.get_int("n_tasks")? as u32;
    let instance = RcpspInstance::new(
        dzn_vars
            .get_1d_int_array("rc")?
            .iter()
            .map(|x| *x as u32)
            .collect::<Vec<_>>(),
        dzn_vars
            .get_1d_int_array("d")?
            .iter()
            .map(|x| *x as u32)
            .collect::<Vec<_>>(),
        dzn_vars
            .get_2d_int_array("rr")?
            .iter()
            .map(|xs| xs.iter().map(|x| *x as u32).collect::<Vec<_>>())
            .collect::<Vec<_>>(),
        dzn_vars
            .get_1d_set_of_int_array("suc")?
            .iter()
            .map(|xs| xs.iter().map(|x| *x as usize).collect::<Vec<_>>())
            .collect::<Vec<_>>(),
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

pub(crate) type SchedulingResult<T> = Result<T, SchedulingError>;

#[derive(Error, Debug)]
#[allow(dead_code)]
pub(crate) enum SchedulingError {
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Failed to parse the input file, details: {0}")]
    MalformedFile(#[from] DznError),
    #[error("Failed to extract a key from the input file, details: {0}")]
    DznMissingValue(#[from] MissingValueError),
    #[error("The file {0} is not supported.")]
    InvalidInstanceFile(String),
    #[error("No file location given")]
    MissingFileError,
    #[error("Inconsistency between `{0}` (value {1}) and `{2}` (value {3})")]
    InconsistentValues(String, i64, String, i64),
}

impl SchedulingError {
    pub(crate) fn invalid_instance(path: impl Display) -> Self {
        Self::InvalidInstanceFile(format!("{}", path))
    }
}
