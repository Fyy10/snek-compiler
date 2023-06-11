use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64, buffer: *mut u64) -> u64;
}

fn snek_str(i : u64, seen : &mut Vec<u64>) -> String {
    if i & 1 == 0 { format!("{}", (i as i64) >> 1) }
    else if i == 0b111 { "true".to_string() }
    else if i == 0b11 { "false".to_string() }
    else if i == 0b1 { "nil".to_string() }
    else if i & 3 == 1 {
        // deal with cyclic values
        if seen.contains(&i) {
            return "(...)".to_string()
        } else {
            seen.push(i);
        }

        let mut tuple_str = String::from("(tuple");
        let addr = (i - 1) as *const u64;
        let size = unsafe { *addr };
        for idx in 1..=size {
            let e = unsafe { *addr.offset(idx as isize) };
            tuple_str.push_str(&format!(" {}", snek_str(e, seen)));
        }
        tuple_str.push_str(&format!(")"));

        seen.pop();
        tuple_str
    }
    else { format!("unknown {}", i) }
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(i : u64) {
    println!("{}", snek_str(i, &mut Vec::<u64>::new()));
}

#[no_mangle]
#[export_name = "\x01snek_error"]
fn snek_error(errcode: i64) {
    // print error message according to writeup
    match errcode {
        1 => eprintln!("Error: invalid argument (type error)"),
        2 => eprintln!("Error: overflow"),
        3 => eprintln!("Error: index out of bound"),
        _ => eprintln!("an unknown error occurred, error code {errcode}")
    }
    std::process::exit(1);
}

#[no_mangle]
#[export_name = "\x01snek_equal"]
// TODO: snek_equal
fn snek_equal(t1 : u64, t2 : u64) -> u64 {
    // TODO: structural equality of t1 and t2
    println!("{:#x}, {:#x}", t1, t2);
    // false
    0b11
}

fn parse_input(input: &str) -> u64 {
    // parse the input string into internal value representation
    // 0b0111 for "true"
    if input == "true" { 0b111 }
    // 0b0011 for "false"
    else if input == "false" { 0b11 }
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

    let heap_size = 100000;
    let mut memory = Vec::<u64>::with_capacity(heap_size);
    let buffer : *mut u64 = memory.as_mut_ptr();

    let i: u64 = unsafe { our_code_starts_here(input, buffer) };
    // println!("{i}");
    snek_print(i);
}
