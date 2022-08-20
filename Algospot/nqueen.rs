fn safe(c: int, cs: &Vec<int>) -> bool {
    let mut r1: int = 0;
    let r0: int = cs.len() as int;
    for &c1 in cs.iter() {
        if c1 == c || r0 - r1 == std::num::abs(c - c1) {
            return false;
        }
        r1 = r1 + 1;
    }
    return true;
}

fn solve(n: int, cs: Vec<int>) -> int {
    let x = cs.len() as int;
    if n == x {
        1
    } else {
        let mut s: int = 0;
        for c in range(0, n) {
            if safe(c, &cs) {
                let mut child: Vec<int> = cs.clone();
                child.push(c);
                s = s + solve(n, child)
            }
        }
        s
    }
}

fn main() {
    println!("{}", solve(12, Vec::new()))
}
