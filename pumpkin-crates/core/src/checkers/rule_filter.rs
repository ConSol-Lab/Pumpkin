use std::sync::OnceLock;

/// The rules that the environment variable `PUMPKIN_CHECK_RULES` selects for checking, as the
/// checker failures print their names, separated by commas.
///
/// When it is unset or empty, every rule is checked.
pub(crate) fn selected_rules() -> &'static [String] {
    static SELECTED: OnceLock<Vec<String>> = OnceLock::new();

    SELECTED.get_or_init(|| {
        std::env::var("PUMPKIN_CHECK_RULES")
            .unwrap_or_default()
            .split(',')
            .map(|name| name.trim().to_owned())
            .filter(|name| !name.is_empty())
            .collect()
    })
}

/// Whether the checkers of the rule named `rule_name` are added and run; see [`selected_rules`].
#[cfg(feature = "checkers")]
pub(crate) fn is_rule_checked(rule_name: &str) -> bool {
    is_selected(selected_rules(), rule_name)
}

#[cfg(feature = "checkers")]
fn is_selected(selected_rules: &[String], rule_name: &str) -> bool {
    selected_rules.is_empty() || selected_rules.iter().any(|name| name == rule_name)
}

#[cfg(test)]
#[cfg(feature = "checkers")]
mod tests {
    use super::*;

    #[test]
    fn every_rule_is_checked_without_a_selection() {
        assert!(is_selected(&[], "nogood"));
    }

    #[test]
    fn only_the_selected_rules_are_checked() {
        let selected = ["nogood".to_owned()];

        assert!(is_selected(&selected, "nogood"));
        assert!(!is_selected(&selected, "linear_le"));
    }
}
