//!
//! Integration tests: compile and execute sample source codes
//!
use std::env;
use std::io::stdout;

fn main() {
    env::set_var("RUST_LOG", "INFO");
    env_logger::init();

    let test_example = "arithmetic";
    
    // Get the current directory
    let current_dir = env::current_dir().expect("Failed to get current directory");
    
    let src_path = current_dir
        .join("integration_tests")
        .join("resources")
        .join(format!("{}.cool", test_example));
    
    // Create test_results directory if it doesn't exist
    let test_results_dir = current_dir
        .join("integration_tests")
        .join("test_results");
    std::fs::create_dir_all(&test_results_dir)
        .expect("Failed to create test_results directory");
        
    let binary_path = test_results_dir.join(test_example);
    let stdlib_path = current_dir
        .join("compiler")
        .join("stdlib");

    compiler::compile(
        src_path.as_path(),
        stdlib_path.as_path(),
        binary_path.as_path(),
        false,
    ).expect(&format!("Cannot compile {}", test_example));

    //let flags: u8 = virtualmachine::FLAG_ENABLE_DEBUGGER | virtualmachine::FLAG_ENABLE_TRACE;
    let flags: u8 = 0;
    virtualmachine::run(binary_path.as_path(), Box::new(stdout()), flags);
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::fs::File;
    use std::path::Path;

    #[test]
    fn test_arithmetic() {
        compile_and_run("arithmetic")
    }

    #[test]
    fn test_bitwise() {
        compile_and_run("bitwise")
    }

    #[test]
    fn test_fields() {
        compile_and_run("fields")
    }

    #[test]
    fn test_if_else() {
        compile_and_run("if_else")
    }

    #[test]
    fn test_list() {
        compile_and_run("list")
    }

    #[test]
    fn test_array() {
        compile_and_run("array")
    }

    #[test]
    fn test_method_call() {
        compile_and_run("method_call")
    }

    #[test]
    fn test_new_object() {
        compile_and_run("new_object")
    }

    #[test]
    fn test_string() {
        compile_and_run("string")
    }

    #[test]
    fn test_var_assign() {
        compile_and_run("var_assign")
    }

    #[test]
    fn test_while() {
        compile_and_run("while")
    }

    #[test]
    fn test_match() {
        compile_and_run("match")
    }

    #[test]
    fn test_hashmap() {
        compile_and_run("hashmap")
    }

    #[test]
    fn test_inheritance() {
        compile_and_run("inheritance")
    }

    #[test]
    fn test_qsort() {
        compile_and_run("qsort")
    }

    fn compile_and_run(test_name: &str) {
        let debug_ast = false;
        let debug_trace = false;

        let src_path = Path::new("./resources/").join(test_name)
            .with_extension("cool");

        // Create test_results directory if it doesn't exist
        let test_results_dir = Path::new("./test_results");
        std::fs::create_dir_all(test_results_dir)
            .expect("Failed to create test_results directory");

        let binary_path = test_results_dir.join(test_name);
        compiler::compile(
            src_path.as_path(),
            Path::new("../compiler/stdlib/"),
            binary_path.as_path(),
            debug_ast,
        )
        .expect("Cannot compile");

        let mut stdout_path = Path::new("./test_results/").join(test_name);
        stdout_path.set_extension("stdout.txt");
        let stdout_file = File::create(stdout_path.as_path()).expect("Cannot create stdout file");

        let mut flags: u8 = 0;
        if debug_trace {
            flags |= virtualmachine::FLAG_ENABLE_TRACE;
        }
        virtualmachine::run(binary_path.as_path(), Box::new(stdout_file), flags);

        let actual_stdout = fs::read_to_string(stdout_path).expect("Cannot read actual stdout");
        let mut expected = Path::new("./resources/").join(test_name);
        expected.set_extension("expected.txt");
        let mut expected_stdout =
            fs::read_to_string(expected).expect("Cannot read expected stdout");
        expected_stdout = expected_stdout.replace("\r\n", "\n");

        assert_eq!(actual_stdout, expected_stdout);
    }
}
