#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum Handsign {
    Rock,
    Scissors,
    Paper
}

use Handsign::*;

impl Handsign {
    fn prey(&self) -> Handsign {
        match self {
            Rock => Scissors,
            Scissors => Paper,
            Paper => Rock,
        }
    }
}

fn winner(hs: &[Handsign]) -> Option<Handsign> {
    let mut set = Vec::new();
    for h in hs.into_iter() {
        if !set.contains(h) {
            set.push(*h)
        }
    }
    match set.as_slice() {
        &[a, b] => Some(if a.prey() == b { a } else { b }),
        _ => None
    }
}

fn main() {
    println!("{:?}", winner(&[Scissors, Rock]));
    println!("{:?}", winner(&[Rock, Scissors]));
    println!("{:?}", winner(&[Rock, Rock]));
    println!("{:?}", winner(&[Rock, Scissors, Paper]));
    println!("{:?}", winner(&[Rock, Scissors, Rock, Scissors]));
}

#[test]
fn decided() {
    assert_eq!(winner(&[Scissors, Rock]), Some(Rock));
}

#[test]
fn undecided_one_kind() {
    assert_eq!(winner(&[Rock, Rock]), None);
}

#[test]
fn undecided_three_kind() {
    assert_eq!(winner(&[Rock, Scissors, Paper]), None);
}

#[test]
fn allow_repeatition() {
    assert_eq!(winner(&[Rock, Scissors, Rock]), Some(Rock));
}
