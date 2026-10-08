//! The settings of the propagators that take parameters ([`PropagatorParameters`]). The fuzzer
//! draws one setting per case and passes it to the FlatZinc compiler.

use pumpkin_core::propagation::PropagatorParameters;
use pumpkin_solver::flatzinc::CompilationOptions;
use pumpkin_solver::propagators::cumulative::options::CumulativeOptions;

/// One setting of the parameters of the propagator of an example, applied through the options of
/// the FlatZinc compiler.
#[derive(Clone, Debug, Default)]
pub struct Setting {
    /// The [`Debug`] text of the parameters, or empty when the propagator takes none.
    pub name: String,
    pub compilation_options: CompilationOptions,
}

impl Setting {
    pub fn is_default(&self) -> bool {
        self.name.is_empty()
    }
}

/// The legal settings of the propagator of the FlatZinc constraint `constraint_name` whose names
/// contain every text in `filters`. A constraint whose propagator takes no parameters has one
/// setting, the default, whatever the filters.
///
/// A propagator that implements [`PropagatorParameters`] and can be built from FlatZinc needs an
/// arm here, which sets its parameters in the [`CompilationOptions`].
pub fn settings(constraint_name: &str, filters: &[String]) -> Vec<Setting> {
    let settings = match constraint_name {
        "pumpkin_cumulative" => settings_of::<CumulativeOptions>(|options, compilation_options| {
            compilation_options.cumulative_options = options;
        }),
        _ => return vec![Setting::default()],
    };

    settings
        .into_iter()
        .filter(|setting| filters.iter().all(|filter| setting.name.contains(filter)))
        .collect()
}

fn settings_of<Parameters: PropagatorParameters>(
    apply: impl Fn(Parameters, &mut CompilationOptions),
) -> Vec<Setting> {
    Parameters::all_legal()
        .into_iter()
        .map(|parameters| {
            let mut compilation_options = CompilationOptions::default();
            let name = format!("{parameters:?}");
            apply(parameters, &mut compilation_options);
            Setting {
                name,
                compilation_options,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filters_narrow_the_settings_of_the_cumulative() {
        let all = settings("pumpkin_cumulative", &[]);
        let naive = settings("pumpkin_cumulative", &["Naive".to_owned()]);
        let naive_per_point = settings(
            "pumpkin_cumulative",
            &["Naive".to_owned(), "TimeTablePerPoint,".to_owned()],
        );

        assert_eq!(all.len(), 144);
        assert_eq!(naive.len(), 48);
        assert_eq!(naive_per_point.len(), 8);
    }

    #[test]
    fn the_name_of_a_setting_selects_only_that_setting() {
        for setting in settings("pumpkin_cumulative", &[]) {
            let selected = settings("pumpkin_cumulative", std::slice::from_ref(&setting.name));
            assert_eq!(selected.len(), 1, "{}", setting.name);
        }
    }

    #[test]
    fn a_constraint_without_parameters_has_only_the_default_setting() {
        let settings = settings("int_lin_le", &["Naive".to_owned()]);

        assert_eq!(settings.len(), 1);
        assert!(settings[0].is_default());
    }
}
