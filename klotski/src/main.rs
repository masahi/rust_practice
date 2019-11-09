// ported from https://github.com/masahi/ocaml_practice/blob/master/mooc/klotski.ml
use std::collections::BTreeSet;

fn find_index<T>(pred: impl FnMut(&T) -> bool, vec: &Vec<T>) -> Option<usize> {
    vec.iter().position(pred)
}

fn flat_map<T, F>(rel: F) -> impl Fn(&Vec<T>) -> Vec<T>
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


fn archive_map<T, F>(r: F, (s,l) : (BTreeSet<T>, Vec<T>)) -> (BTreeSet<T>, Vec<T>)
where
    F: Fn(&T) -> Vec<T> + Copy,
    T: Clone + Ord
{
    l.into_iter().fold((s, vec![]), |(seen, new_elts), x| {
        r(&x).into_iter().fold((seen, new_elts), |(mut set, mut frontiers), elt| {
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
    T: Clone + Ord
{
    fn iter<T, F, P>(r: F, p: P, s: BTreeSet<T>, mut l: Vec<T>) -> T
    where
        F: Fn(&T) -> Vec<T> + Copy,
        P: FnMut(&T) -> bool + Copy,
        T: Clone + Ord
    {
        match find_index(p, &l) {
            Some(ind) => l.remove(ind),
            None => {
                let (s, l) = archive_map(r, (s, l));
                iter(r, p, s, l)
            }
        }
    }
    let mut init_set = BTreeSet::new();
    init_set.insert(a.clone());
    iter(r, p, init_set, vec![a])
}

fn solve_path<T, F, P>(r: F, mut p: P, a: T) -> Vec<T>
where
    F: Fn(&T) -> Vec<T> + Copy,
    P: FnMut(&T) -> bool + Copy,
    T: Clone + Ord
{
    let path_rel = move |path: &Vec<T>| {
        match path.last() {
            None => vec![],
            Some(last) => {
                let new_confs = r(last);
                new_confs.into_iter().map(|conf| {
                    let mut p = path.clone();
                    p.push(conf);
                    p
                })
                .collect()
            }
        }
    };
    let path_prop = move |path: &Vec<T>| {
        match path.last() {
            None => false,
            Some(last) => p(last)
        }
    };
    solve(path_rel, path_prop, vec![a])
}

fn main() {
    let near = |n: &i32| vec![*n - 2, *n - 1, *n, *n + 1, *n + 2];
    let new_rel = flat_map(near);
    let out = new_rel(&vec![2, 3, 4]);
    for elem in out.iter() {
        println!("{}", elem);
    }
    println!("{:?}", out);
    let sol = solve(near, |&x| x == 12, 0);
    println!("sol {}", sol);
    let sol_path = solve_path(near, |&x| x == 12, 0);
    println!("{:?}", sol_path);
}
