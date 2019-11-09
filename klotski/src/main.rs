// ported from https://github.com/masahi/ocaml_practice/blob/master/mooc/klotski.ml
use std::cmp::Ordering;
use std::collections::BTreeSet;

fn find_index<T>(pred: impl FnMut(&T) -> bool, vec: &Vec<T>) -> Option<usize> {
    vec.iter().position(pred)
}

/* fn flat_map<T, F>(rel: F) -> impl Fn(&Vec<T>) -> Vec<T>
where
    F: Fn(&T) -> Vec<T> + Copy,
{
    move |conf_list| conf_list.iter().flat_map(rel).collect()
}

fn solve_slow<T, F, P>(r: F, p: P, a: T) -> T
where
    F: Fn(&T) -> Vec<T> + Copy,
    P: FnMut(&T) -> bool + Copy,
{
    fn iter<T, F, P>(mut configs: Vec<T>, r: F, p: P) -> T
    where
        F: Fn(&T) -> Vec<T> + Copy,
        P: FnMut(&T) -> bool + Copy,
    {
        match find_index(p, &configs) {
            Some(ind) => configs.remove(ind),
            None => iter(flat_map(r)(&configs), r, p),
        }
    }
    iter(vec![a], r, p)
}
 */
type Set<T> = BTreeSet<T>;

fn archive_map<T, F>(r: F, (s, l): (Set<T>, Vec<T>)) -> (Set<T>, Vec<T>)
where
    F: Fn(&T) -> Vec<T>,
    T: Clone + Ord,
{
    l.into_iter().fold((s, vec![]), |(seen, new_elts), x| {
        r(&x)
            .into_iter()
            .fold((seen, new_elts), |(mut set, mut frontiers), elt| {
                if set.contains(&elt) {
                    (set, frontiers)
                } else {
                    frontiers.push(elt.clone());
                    set.insert(elt);
                    (set, frontiers)
                }
            })
    })
}

fn solve<T, F, P>(r: F, p: P, a: T) -> T
where
    F: Fn(&T) -> Vec<T> + Copy,
    P: FnMut(&T) -> bool + Copy,
    T: Clone + Ord,
{
    fn iter<T, F, P>(r: F, p: P, s: Set<T>, mut l: Vec<T>) -> T
    where
        F: Fn(&T) -> Vec<T> + Copy,
        P: FnMut(&T) -> bool + Copy,
        T: Clone + Ord,
    {
        match find_index(p, &l) {
            Some(ind) => l.remove(ind),
            None => {
                let (s, l) = archive_map(r, (s, l));
                iter(r, p, s, l)
            }
        }
    }
    let mut init_set = Set::new();
    init_set.insert(a.clone());
    iter(r, p, init_set, vec![a])
}

fn solve_path<T, F, P>(r: F, mut p: P, a: T) -> Vec<T>
where
    F: Fn(&T) -> Vec<T> + Copy,
    P: FnMut(&T) -> bool + Copy,
    T: Clone + Ord,
{
    #[derive(PartialEq, Eq, Clone)]
    struct SolutionPath<T>(Vec<T>);

    impl<T: Ord> SolutionPath<T> {
        fn new() -> SolutionPath<T> {
            SolutionPath(vec![])
        }

        fn push(&mut self, elem: T) {
            self.0.push(elem)
        }

        fn last(&self) -> Option<&T> {
            self.0.last()
        }
    }

    impl<T: Ord> Ord for SolutionPath<T> {
        fn cmp(&self, other: &Self) -> Ordering {
            match (self.last(), other.last()) {
                (None, None) => Ordering::Equal,
                (Some(_), None) => Ordering::Greater,
                (None, Some(_)) => Ordering::Less,
                (Some(x), Some(y)) => x.cmp(y),
            }
        }
    }

    impl<T: Ord> PartialOrd for SolutionPath<T> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    let path_rel = move |path: &SolutionPath<T>| match path.last() {
        None => vec![],
        Some(last) => {
            let new_confs = r(last);
            new_confs
                .into_iter()
                .map(|conf| {
                    let mut p = path.clone();
                    p.push(conf);
                    p
                })
                .collect()
        }
    };
    let path_prop = move |path: &SolutionPath<T>| match path.last() {
        None => false,
        Some(last) => p(last),
    };
    let mut init = SolutionPath::new();
    init.push(a);
    let sol = solve(path_rel, path_prop, init);
    sol.0
}

trait Puzzle<Conf, Move> {
    fn apply_move(&self, c: &Conf, m: &Move) -> Conf;
    fn possible_move(&self, c: &Conf) -> Vec<Move>;
    fn is_final(&self, c: &Conf) -> bool;
}

fn solve_puzzle<Conf, Move, P>(puzzle: P, init_conf: Conf) -> Vec<Conf>
where
    Conf: Clone + Ord,
    P: Puzzle<Conf, Move> + Copy
{
    let rel = move |conf: &Conf| {
        let moves = puzzle.possible_move(conf);
        moves.into_iter().map(|mv| puzzle.apply_move(conf, &mv)).collect()
    };
    solve_path(rel, |c| puzzle.is_final(c), init_conf)
}

fn main() {
    let near = |n: &i32| vec![*n - 2, *n - 1, *n, *n + 1, *n + 2];
    let sol = solve(near, |&x| x == 12, 0);
    println!("sol {}", sol);
    let sol_path = solve_path(near, |&x| x == 12, 0);
    println!("sol path {:?}", sol_path);
}
