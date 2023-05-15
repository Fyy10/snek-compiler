use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(i : u64) {
    if i & 1 == 0 { println!("{}", (i as i64) >> 1); }
    else if i == 3 { println!("true"); }
    else if i == 1 { println!("false"); }
    else { println!("unknown {}", i); }
}

#[no_mangle]
#[export_name = "\x01snek_error"]
fn snek_error(errcode: i64) {
    // print error message according to writeup
    match errcode {
        1 => eprintln!("Error: invalid argument"),
        2 => eprintln!("Error: overflow"),
        _ => eprintln!("an unknown error occurred, error code {errcode}")
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    // parse the input string into internal value representation
    // 0b0011 for "true"
    if input == "true" { 3 }
    // 0b0001 for "false"
    else if input == "false" { 1 }
    // other (may be number or invalid string)
    else {
        // input.parse::<u64>().unwrap() << 1
        let result = input.parse::<i64>();
        if result.is_err() {
            panic!("Invalid: \"{}\" cannot be parsed as i64", input)
        }
        let val = result.unwrap();
        // println!("0b{:64b}", val);
        if val < -4611686018427387904 || val > 4611686018427387903 {
            panic!("Invalid: number out of bound")
        }
        (val as u64) << 1
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    // println!("{i}");
    snek_print(i);
}
