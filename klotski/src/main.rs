fn find<T>(pred: impl FnMut(&&T) -> bool, vec: &Vec<T>) -> Option<&T> {
    vec.iter().find(pred)
}

fn flat_map<T, F>(rel: F) -> impl Fn(&Vec<T>) -> Vec<T>
where
    F: Fn(&T) -> Vec<T> + Copy,
{
    move |conf_list| conf_list.iter().flat_map(rel).collect()
}

fn solve<T, F, P>(r: F, p: P, a: T) -> T
where
    F: Fn(&T) -> Vec<T> + Copy,
    P: FnMut(&&T) -> bool + Copy,
    T: Clone
{
    fn iter<T, F, P>(configs: Vec<T>, r: F, p: P) -> T
    where
        F: Fn(&T) -> Vec<T> + Copy,
        P: FnMut(&&T) -> bool + Copy,
        T: Clone
    {
        match find(p, &configs) {
            Some(x) => (*x).clone(),
            None => iter(flat_map(r)(&configs), r, p),
        }
    }
    iter(vec![a], r, p)
}

fn main() {
    let near = |n: &i32| vec![*n - 2, *n - 1, *n, *n + 1, *n + 2];
    let new_rel = flat_map(near);
    let out = new_rel(&vec![2, 3, 4]);
    for elem in out.iter() {
        println!("{}", elem);
    }
    let sol = solve(near, |&&x| x == 12, 0);
    println!("sol {}", sol);
}
