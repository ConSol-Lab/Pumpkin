use std::collections::HashMap;
use std::env;

pub struct ArgumentHandler {
    categories: HashMap<String, Category>,
    arguments_integer: HashMap<String, IntegerEntry>,
    arguments_string: HashMap<String, StringEntry>,
    arguments_float: HashMap<String, FloatEntry>,
    arguments_bool: HashMap<String, BoolEntry>,
}

impl ArgumentHandler {
    pub fn new() -> ArgumentHandler {
        ArgumentHandler {
            categories: HashMap::new(),
            arguments_integer: HashMap::new(),
            arguments_string: HashMap::new(),
            arguments_float: HashMap::new(),
            arguments_bool: HashMap::new(),
        }
    }

    pub fn print_if_empty_arguments_and_exit(&self) {
        if env::args().skip(0).count() <= 1 {
            println!("----------------------");
            println!("No arguments found. Pass the arguments '-help' to see info.");
            std::process::exit(0);
        }
    }

    pub fn print_help_summary_if_needed_and_exit(&self) {
        if !env::args().skip(1).any(|arg| arg.starts_with("-help")) {
            return;
        }

        println!("----------------------");
        println!("Displaying argument help information.");
        for entry in &self.categories {
            println!("----------------------");
            println!("Category: {}. {}", *entry.0, (*entry.1).short_description);
            for arg in &(*entry.1).arguments {
                match arg.data_type {
                    DataType::Integer => {
                        let arg_info = self.arguments_integer.get(&arg.name).unwrap();

                        println!("\t-{}. Integer. {}", arg.name, arg_info.short_description);
                        println!("\t\tDefault: {}", arg_info.default_value);
                        println!(
                            "\t\tRange = [{}, {}]",
                            arg_info.min_value, arg_info.max_value
                        );
                    }
                    DataType::Float => {
                        let arg_info = self.arguments_float.get(&arg.name).unwrap();

                        println!("\t-{}. Float. {}", arg.name, arg_info.short_description);
                        println!("\t\tDefault: {}", arg_info.default_value);
                        println!(
                            "\t\tRange = [{}, {}]",
                            arg_info.min_value, arg_info.max_value
                        );
                    }
                    DataType::String => {
                        let arg_info = self.arguments_string.get(&arg.name).unwrap();

                        println!("\t-{}. String. {}", arg.name, arg_info.short_description);
                        println!("\t\tDefault: {}", arg_info.default_value);
                        if arg_info.allowed_values.is_empty() {
                            println!("\t\tAllowed values = [any string, no restrictions]");
                        } else {
                            println!(
                                "\t\tAllowed values = {{{}}}",
                                arg_info.allowed_values.join(", ")
                            );
                        }
                    }
                    DataType::Bool => {
                        let arg_info = self.arguments_bool.get(&arg.name).unwrap();

                        println!("\t-{}. Bool. {}", arg.name, arg_info.short_description);
                        println!("\t\tDefault: {}", arg_info.default_value);
                    }
                }
            }
        }
        println!("----------------------");
        std::process::exit(0);
    }

    pub fn print_argument_values(&self) {
        println!("----------------------");
        println!("Displaying current argument values.");
        for entry in &self.categories {
            println!("----------------------");
            println!("Category: {}.", *entry.0);
            for arg in &(*entry.1).arguments {
                match arg.data_type {
                    DataType::Integer => {
                        let arg_info = self.arguments_integer.get(&arg.name).unwrap();
                        println!("\t-{}={}", arg.name, arg_info.current_value);
                    }
                    DataType::Float => {
                        let arg_info = self.arguments_float.get(&arg.name).unwrap();
                        println!("\t-{}={}", arg.name, arg_info.current_value);
                    }
                    DataType::String => {
                        let arg_info = self.arguments_string.get(&arg.name).unwrap();
                        println!("\t-{}={}", arg.name, arg_info.current_value);
                    }
                    DataType::Bool => {
                        let arg_info = self.arguments_bool.get(&arg.name).unwrap();
                        println!("\t-{}={}", arg.name, arg_info.current_value);
                    }
                }
            }
        }
        println!("----------------------");
    }

    pub fn print_arguments_different_from_default(&self) {
        println!("----------------------");
        println!("Displaying arguments whose value differs from the default.");
        let mut at_least_one_argument_is_different = false;
        for entry in &self.categories {
            println!("----------------------");
            println!("Category: {}.", *entry.0);
            for arg in &(*entry.1).arguments {
                match arg.data_type {
                    DataType::Integer => {
                        let arg_info = self.arguments_integer.get(&arg.name).unwrap();
                        if arg_info.current_value != arg_info.default_value {
                            at_least_one_argument_is_different = true;
                            println!(
                                "\t-{}={} (default: {})",
                                arg.name, arg_info.current_value, arg_info.default_value
                            );
                        }
                    }
                    DataType::Float => {
                        let arg_info = self.arguments_float.get(&arg.name).unwrap();
                        if arg_info.current_value != arg_info.default_value {
                            at_least_one_argument_is_different = true;
                            println!(
                                "\t-{}={} (default: {})",
                                arg.name, arg_info.current_value, arg_info.default_value
                            );
                        }
                    }
                    DataType::String => {
                        let arg_info = self.arguments_string.get(&arg.name).unwrap();
                        if arg_info.current_value != arg_info.default_value {
                            at_least_one_argument_is_different = true;
                            println!(
                                "\t-{}={} (default: {})",
                                arg.name, arg_info.current_value, arg_info.default_value
                            );
                        }
                    }
                    DataType::Bool => {
                        let arg_info = self.arguments_bool.get(&arg.name).unwrap();
                        if arg_info.current_value != arg_info.default_value {
                            at_least_one_argument_is_different = true;
                            println!(
                                "\t-{}={} (default: {})",
                                arg.name, arg_info.current_value, arg_info.default_value
                            );
                        }
                    }
                }
            }
        }

        if !at_least_one_argument_is_different {
            println!("\tAll arguments are set to their default values.");
        }
        println!("----------------------");
    }

    fn get_argument_type(&self, name: &str) -> Option<DataType> {
        if self.arguments_integer.contains_key(name) {
            Some(DataType::Integer)
        } else if self.arguments_bool.contains_key(name) {
            Some(DataType::Bool)
        } else if self.arguments_float.contains_key(name) {
            Some(DataType::Float)
        } else if self.arguments_string.contains_key(name) {
            Some(DataType::String)
        } else {
            None
        }
    }

    pub fn set_integer_argument(&mut self, argument_name: &str, value: i64) {
        assert!(self.arguments_integer.contains_key(argument_name));

        let entry = self.arguments_integer.get_mut(argument_name).unwrap();

        assert!(
            entry.min_value <= value && value <= entry.max_value,
            "The value '{value}' supplied for argument '{argument_name}' is not within the allowed range [{}, {}]",
            entry.min_value,
            entry.max_value
        );

        entry.current_value = value;
    }

    pub fn set_float_argument(&mut self, argument_name: &str, value: f64) {
        assert!(self.arguments_float.contains_key(argument_name));

        let entry = self.arguments_float.get_mut(argument_name).unwrap();

        assert!(
            entry.min_value <= value && value <= entry.max_value,
            "The value '{value}' supplied for argument '{argument_name}' is not within the allowed range [{}, {}]",
            entry.min_value,
            entry.max_value
        );

        entry.current_value = value;
    }

    pub fn set_string_argument(&mut self, argument_name: &str, value: &str) {
        assert!(self.arguments_string.contains_key(argument_name));

        let arg_info = self.arguments_string.get_mut(argument_name).unwrap();

        assert!(
            arg_info.allowed_values.is_empty() ||
            arg_info.allowed_values.iter().any(|s| *s == value),
            "The value '{value}' supplied for argument '{argument_name}' is not within the allowed values ({{{}}}).",
            arg_info.allowed_values.join(", ")
        );

        arg_info.current_value = value.to_string();
    }

    pub fn set_bool_argument(&mut self, argument_name: &str, value: bool) {
        assert!(self.arguments_bool.contains_key(argument_name));

        self.arguments_bool
            .get_mut(argument_name)
            .unwrap()
            .current_value = value;
    }

    pub fn parse_command_line_arguments(&mut self) {
        //skip the first argument since it does not contain user arguments
        for argument in env::args().skip(1) {
            assert!(!argument.is_empty(), "Found empty command line argument.");
            assert!(
                argument.starts_with('-'),
                "Each command line argument is expected to start with '-'."
            );
            assert!(
                argument.find('=').is_some(),
                "Found command line argument without '='"
            );

            let argument_name = &argument.to_string()[1..argument.find('=').unwrap()];
            let raw_argument_value = &argument.to_string()[(argument.find('=').unwrap() + 1)..];
            let argument_type = self.get_argument_type(argument_name);
            assert!(argument_type.is_some(), "Unknown argument: {argument_name}");

            match argument_type.unwrap() {
                DataType::Integer => {
                    let value = raw_argument_value.parse::<i64>().unwrap_or_else(|_| {
                        panic!("{}", format!("Cannot convert argument value '{raw_argument_value}' into an integer.")) 
                    });
                    self.set_integer_argument(argument_name, value);
                }
                DataType::Float => {
                    let value = raw_argument_value.parse::<f64>().unwrap_or_else(|_| {
                        panic!("{}", format!("Cannot convert argument value '{raw_argument_value}' into a float.")) 
                    });

                    self.set_float_argument(argument_name, value);
                }
                DataType::String => {
                    self.set_string_argument(argument_name, raw_argument_value);
                }
                DataType::Bool => {
                    let value = raw_argument_value.parse::<bool>().unwrap_or_else(|_| {
                        panic!("{}", format!("Cannot convert argument value '{raw_argument_value}' into a bool. 
                             Remember to use 'true' and 'false' for bools rather than numeric values.")) 
                    });

                    self.set_bool_argument(argument_name, value);
                }
            }
        }
    }

    pub fn define_new_category(&mut self, category_name: &str, short_description: &str) {
        assert!(!category_name.is_empty() && !short_description.is_empty());
        assert!(
            !self.categories.contains_key(category_name),
            "Category '{}' already exists, cannot create it twice.",
            category_name
        );

        self.categories.insert(
            category_name.to_string(),
            Category {
                name: category_name.to_string(),
                short_description: short_description.to_string(),
                arguments: vec![],
            },
        );
    }

    pub fn define_integer_argument(
        &mut self,
        argument_name: &str,
        category_name: &str,
        short_description: &str,
        default_value: i64,
        min_value: i64,
        max_value: i64,
    ) {
        self.basic_checks_on_input(argument_name, category_name, short_description);

        self.categories
            .get_mut(category_name)
            .unwrap()
            .arguments
            .push(PairNameType {
                name: argument_name.to_string(),
                data_type: DataType::Integer,
            });

        self.arguments_integer.insert(
            argument_name.to_string(),
            IntegerEntry {
                name: argument_name.to_string(),
                short_description: short_description.to_string(),
                category_name: category_name.to_string(),
                default_value,
                current_value: default_value,
                min_value,
                max_value,
            },
        );
    }

    pub fn define_string_argument(
        &mut self,
        argument_name: &str,
        category_name: &str,
        short_description: &str,
        default_value: &str,
        allowed_values: &[&str],
    ) {
        self.basic_checks_on_input(argument_name, category_name, short_description);
        assert!(
            allowed_values.is_empty() || allowed_values.iter().any(|s| *s == default_value),
            "Default value must be within the allowed values for the parameter '{}'.",
            argument_name
        );

        self.categories
            .get_mut(category_name)
            .unwrap()
            .arguments
            .push(PairNameType {
                name: argument_name.to_string(),
                data_type: DataType::String,
            });

        self.arguments_string.insert(
            argument_name.to_string(),
            StringEntry {
                name: argument_name.to_string(),
                short_description: short_description.to_string(),
                category_name: category_name.to_string(),
                default_value: default_value.to_string(),
                current_value: default_value.to_string(),
                allowed_values: allowed_values.iter().map(|s| s.to_string()).collect(),
            },
        );
    }

    pub fn define_bool_argument(
        &mut self,
        argument_name: &str,
        category_name: &str,
        short_description: &str,
        default_value: bool,
    ) {
        self.basic_checks_on_input(argument_name, category_name, short_description);

        self.categories
            .get_mut(category_name)
            .unwrap()
            .arguments
            .push(PairNameType {
                name: argument_name.to_string(),
                data_type: DataType::Bool,
            });

        self.arguments_bool.insert(
            argument_name.to_string(),
            BoolEntry {
                name: argument_name.to_string(),
                short_description: short_description.to_string(),
                category_name: category_name.to_string(),
                default_value,
                current_value: default_value,
            },
        );
    }

    pub fn define_float_argument(
        &mut self,
        argument_name: &str,
        category_name: &str,
        short_description: &str,
        default_value: f64,
        min_value: f64,
        max_value: f64,
    ) {
        self.basic_checks_on_input(argument_name, category_name, short_description);

        self.categories
            .get_mut(category_name)
            .unwrap()
            .arguments
            .push(PairNameType {
                name: argument_name.to_string(),
                data_type: DataType::Float,
            });

        self.arguments_float.insert(
            argument_name.to_string(),
            FloatEntry {
                name: argument_name.to_string(),
                short_description: short_description.to_string(),
                category_name: category_name.to_string(),
                default_value,
                current_value: default_value,
                min_value,
                max_value,
            },
        );
    }

    pub fn get_integer_argument(&self, argument_name: &str) -> i64 {
        assert!(
            self.arguments_integer.contains_key(argument_name),
            "Cannot retrieve undefined parameter '{}'.",
            argument_name
        );

        self.arguments_integer[&argument_name.to_string()].current_value
    }

    pub fn get_float_argument(&self, argument_name: &str) -> f64 {
        assert!(
            self.arguments_float.contains_key(argument_name),
            "Cannot retrieve undefined parameter '{}'.",
            argument_name
        );

        self.arguments_float[&argument_name.to_string()].current_value
    }

    pub fn get_string_argument(&self, argument_name: &str) -> String {
        assert!(
            self.arguments_string.contains_key(argument_name),
            "Cannot retrieve undefined parameter '{}'.",
            argument_name
        );

        self.arguments_string[&argument_name.to_string()]
            .current_value
            .clone()
    }

    pub fn get_bool_argument(&self, argument_name: &str) -> bool {
        assert!(
            self.arguments_bool.contains_key(argument_name),
            "Cannot retrieve undefined parameter '{}'.",
            argument_name
        );

        self.arguments_bool[&argument_name.to_string()].current_value
    }

    fn basic_checks_on_input(
        &self,
        argument_name: &str,
        category_name: &str,
        short_description: &str,
    ) {
        assert!(
            !category_name.is_empty()
                && !argument_name.is_empty()
                && !short_description.is_empty(),
            "Empty strings for the category or parameter name are not allowed, nor are empty descriptions allowed."
        );

        assert!(
            self.categories.contains_key(category_name),
            "Need to define the category '{}' before adding the integer parameter '{}'.",
            category_name,
            argument_name
        );

        assert!(
            !self.arguments_integer.contains_key(argument_name)
                && !self.arguments_float.contains_key(argument_name)
                && !self.arguments_string.contains_key(argument_name)
                && !self.arguments_bool.contains_key(argument_name),
            "Parameter '{}' already defined, cannot define it twice.",
            argument_name
        );
    }
}

enum DataType {
    Integer,
    Float,
    String,
    Bool,
}

struct PairNameType {
    name: String,
    data_type: DataType,
}

struct IntegerEntry {
    name: String,
    short_description: String,
    category_name: String,
    default_value: i64,
    current_value: i64,
    min_value: i64,
    max_value: i64,
}

struct FloatEntry {
    name: String,
    short_description: String,
    category_name: String,
    default_value: f64,
    current_value: f64,
    min_value: f64,
    max_value: f64,
}

struct StringEntry {
    name: String,
    short_description: String,
    category_name: String,
    default_value: String,
    current_value: String,
    allowed_values: Vec<String>,
}

struct BoolEntry {
    name: String,
    short_description: String,
    category_name: String,
    default_value: bool,
    current_value: bool,
}

struct Category {
    name: String,
    short_description: String,
    arguments: Vec<PairNameType>,
}
