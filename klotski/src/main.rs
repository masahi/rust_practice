//ported from https://github.com/masahi/ocaml_practice/blob/master/mooc/klotski.ml
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::ops::{Deref, DerefMut};

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

fn expand_frontiers<T, F>(r: F, (s, l): (Set<T>, Vec<T>)) -> (Set<T>, Vec<T>)
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
    fn iter<T, F, P>(r: F, p: P, s: Set<T>, mut l: Vec<T>, round: usize) -> T
    where
        F: Fn(&T) -> Vec<T> + Copy,
        P: FnMut(&T) -> bool + Copy,
        T: Clone + Ord,
    {
        match find_index(p, &l) {
            Some(ind) => l.remove(ind),
            None => {
                let (s, l) = expand_frontiers(r, (s, l));
                println!("Round {}, frontier size {}", round, l.len());
                iter(r, p, s, l, round + 1)
            }
        }
    }
    let mut init_set = Set::new();
    init_set.insert(a.clone());
    iter(r, p, init_set, vec![a], 0)
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

//fn solve_puzzle<Conf, Move, P>(puzzle: P, init_conf: Conf) -> Vec<Conf>
fn solve_puzzle<Conf, Move, P>(puzzle: P, init_conf: Conf) -> Conf
where
    Conf: Clone + Ord,
    P: Puzzle<Conf, Move> + Copy,
{
    let rel = move |conf: &Conf| {
        let moves = puzzle.possible_move(conf);
        moves
            .into_iter()
            .map(|mv| puzzle.apply_move(conf, &mv))
            .collect()
    };
    solve(rel, |c| puzzle.is_final(c), init_conf)
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum PieceKind {
    S,
    H,
    V,
    C,
    X,
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct Piece {
    kind: PieceKind,
    index: u8,
}

impl Piece {
    fn to_string(&self) -> String {
        let ch = match self.kind {
            PieceKind::S => "S",
            PieceKind::H => "H",
            PieceKind::C => "C",
            PieceKind::V => "V",
            PieceKind::X => "X",
        };
        format!("({}, {})", ch, self.index)
    }
}

static X: Piece = Piece {
    kind: PieceKind::X,
    index: 0,
};
static S: Piece = Piece {
    kind: PieceKind::S,
    index: 0,
};
static H: Piece = Piece {
    kind: PieceKind::H,
    index: 0,
};
static C0: Piece = Piece {
    kind: PieceKind::C,
    index: 0,
};
static C1: Piece = Piece {
    kind: PieceKind::C,
    index: 1,
};
static C2: Piece = Piece {
    kind: PieceKind::C,
    index: 2,
};
static C3: Piece = Piece {
    kind: PieceKind::C,
    index: 3,
};
static V0: Piece = Piece {
    kind: PieceKind::V,
    index: 0,
};
static V1: Piece = Piece {
    kind: PieceKind::V,
    index: 1,
};
static V2: Piece = Piece {
    kind: PieceKind::V,
    index: 2,
};
static V3: Piece = Piece {
    kind: PieceKind::V,
    index: 3,
};

impl Ord for Piece {
    fn cmp(
        &self,
        Piece {
            kind: k2,
            index: ind2,
        }: &Self,
    ) -> Ordering {
        let kind_to_int = |k| match k {
            PieceKind::S => 5,
            PieceKind::H => 4,
            PieceKind::C => 3,
            PieceKind::V => 2,
            PieceKind::X => 1,
        };
        if self.kind == *k2 {
            self.index.cmp(&ind2)
        } else {
            kind_to_int(self.kind).cmp(&kind_to_int(*k2))
        }
    }
}

impl PartialOrd for Piece {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Eq, PartialEq, Clone, Copy)]
struct Board([[Piece; 4]; 5]);
impl Ord for Board {
    fn cmp(&self, other: &Self) -> Ordering {
        let b1 = self.0;
        let b2 = other.0;
        for i in 0..5 {
            for j in 0..4 {
                let c = b1[i][j].cmp(&b2[i][j]);
                if c != Ordering::Equal {
                    return c;
                }
            }
        }
        Ordering::Equal
    }
}

impl PartialOrd for Board {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Deref for Board {
    type Target = [[Piece; 4]; 5];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Board {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone, Copy, PartialEq)]
struct Direction(i8, i8);

#[derive(Clone, Copy, PartialEq)]
struct Pos(u8, u8);

#[derive(Clone, PartialEq)]
struct Move {
    piece: Piece,
    next_occupied_pos: Vec<(u8, u8)>,
    next_vacant_pos: Vec<(u8, u8)>,
}

#[derive(Copy, Clone)]
struct Klotski;

fn move_piece(board: &Board, piece: Piece, pos: Pos, dir: Direction) -> Option<Move> {
    let vec_diff = |v1: &Vec<(u8, u8)>, v2: &Vec<(u8, u8)>| {
        v1.iter()
            .filter(|elt| v2.iter().find(|el2| el2 == elt).is_none())
            .cloned()
            .collect()
    };
    let occupied_pos = |i, j| match piece.kind {
        PieceKind::S => vec![(i, j), (i + 1, j), (i, j + 1), (i + 1, j + 1)],
        PieceKind::H => vec![(i, j), (i, j + 1)],
        PieceKind::V => vec![(i, j), (i + 1, j)],
        PieceKind::C => vec![(i, j)],
        _ => unreachable!(),
    };
    let diff_occupied_pos = |i, j| {
        let current = occupied_pos(i, j);
        let next = occupied_pos((i as i8 + dir.0) as u8, (j as i8 + dir.1) as u8);
        let next_occupied_pos = vec_diff(&next, &current);
        let next_vacant_pos = vec_diff(&current, &next);
        (next_occupied_pos, next_vacant_pos)
    };
    let can_move = |next_occupied: &Vec<(u8, u8)>| {
        let in_bound = |i, j| i < 5 && j < 4;
        next_occupied
            .iter()
            .all(|(i, j)| in_bound(*i, *j) && board[*i as usize][*j as usize] == X)
    };
    let is_dir_safe = |i, j| !((i == 0 && dir.0 == -1) || (j == 0 && dir.1 == -1));
    if is_dir_safe(pos.0, pos.1) {
        let (next_occupied_pos, next_vacant_pos) = diff_occupied_pos(pos.0, pos.1);
        if can_move(&next_occupied_pos) {
            Some(Move {
                piece,
                next_occupied_pos,
                next_vacant_pos,
            })
        } else {
            None
        }
    } else {
        None
    }
}

fn get_piece_positions(board: &Board) -> Vec<(Piece, Pos)> {
    let mut pairs = Vec::new();
    let mut seen = Set::new();
    for i in 0..5 {
        for j in 0..4 {
            let piece = board[i][j];
            if piece != X && !seen.contains(&piece) {
                pairs.push((piece, Pos(i as u8, j as u8)));
                seen.insert(piece);
            }
        }
    }
    pairs
}

impl Puzzle<Board, Move> for Klotski {
    fn apply_move(
        &self,
        board: &Board,
        Move {
            piece,
            next_occupied_pos,
            next_vacant_pos,
        }: &Move,
    ) -> Board {
        let mut board_copy = board.clone();
        next_occupied_pos
            .iter()
            .for_each(|(i, j)| board_copy[*i as usize][*j as usize] = *piece);
        next_vacant_pos
            .iter()
            .for_each(|(i, j)| board_copy[*i as usize][*j as usize] = X);
        board_copy
    }
    fn possible_move(&self, b: &Board) -> Vec<Move> {
        let get_moves = |(p, pos)| -> Vec<Move> {
            let directions = vec![
                Direction(0, 1),
                Direction(0, -1),
                Direction(1, 0),
                Direction(-1, 0),
            ];
            directions
                .into_iter()
                .filter_map(|dir| move_piece(b, p, pos, dir))
                .collect()
        };
        get_piece_positions(b)
            .into_iter()
            .flat_map(get_moves)
            .collect()
    }
    fn is_final(&self, b: &Board) -> bool {
        b[3][1] == S && b[3][2] == S && b[4][1] == S && b[3][2] == S
    }
}

//fn solve_klotski(initial_board: Board) -> Vec<Board> {
fn solve_klotski(initial_board: Board) -> Board {
    solve_puzzle(Klotski, initial_board)
}

fn print_board(board: &Board) {
    for i in 0..5 {
        for j in 0..4 {
            print!("{} ", board[i][j].to_string())
        }
        println!("")
    }
    println!("")
}

fn main() {
    let initial_board_simpler = [
        [C2, S, S, C1],
        [C0, S, S, C3],
        [V1, V2, V3, V0],
        [V1, V2, V3, V0],
        [X, X, X, X],
    ];

    let initial_board = [
        [V0, S, S, V1],
        [V0, S, S, V1],
        [V2, H, H, V3],
        [V2, C0, C1, V3],
        [C2, X, X, C3],
    ];

    let trivial_board = [
        [X, S, S, X],
        [X, S, S, X],
        [X, X, X, X],
        [X, X, X, X],
        [X, X, X, X],
    ];

    let sol = solve_klotski(Board(initial_board_simpler));
    //sol.iter().for_each(print_board)
    print_board(&sol);
}
