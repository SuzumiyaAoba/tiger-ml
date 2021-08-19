(*
 Red-Black Trees

## 右回転

      X ●            Y ●
       / \            / \
    Y ●   × Z ===> A ×   ● X
     / \                / \
  A ×   × W          W ×   × Z


## 左回転

      X ●            Y ●
       / \            / \
    Y ●   × Z ===> A ×   ● X
     / \                / \
  A ×   × W          W ×   × Z

*)

datatype color = RED | BLACK;

datatype 'a tree = LEAF
		 | TREE of color * 'a tree * 'a * 'a tree;

fun inverse (RED) = BLACK
  | inverse (BLACK) = RED;

(*

       Q ●                P ○
        / \     右回転      / \
     P ○   × Qᵣ =====> Pₗ ●   ● Q
      / \                    / \
  Pₗ ○   × Pᵣ            Pᵣ ×   × Qᵣ
*)
fun right_rotate (TREE (qc, TREE (pc, pl, pk, pr), qk, qr)) =
    TREE (pc, pl, pk, TREE (qc, pr, qk, qr));

(*
      Q ●                    P ○
       / \       左回転        / \
   Qₗ ×   ○ P    =====>    Q ●   ● Pᵣ
         / \                / \
     Pₗ ×   ○ Pᵣ        Qₗ ×   × Pₗ
*)
fun left_rotate (TREE (qc, ql, qk, (TREE (pc, pl, pk, pr)))) =
    TREE (pc, TREE (qc, ql, qk, pl), qk, pr);

fun toBlack (TREE (c, l, k, r)) = TREE (BLACK, l, k, r)
  | toBlack LEAF = LEAF;

(*
(1) right_rotate(G);

        G ●                 P ○
         / \     右回転      /   \
      P ○   × Gᵣ =====> N ●       ● G
       / \               / \     / \
    N ○   × Pᵣ          ×   ×   ×   ×
     / \                Nₗ  Nᵣ  Pᵣ  Gᵣ
 Nₗ ×   × Nᵣ

(2) left_rotate(P); right_rorate(G);

        G ●                   G ●                    N ○
         / \     左回転        /   \     右回転        /   \
      P ○   × Gᵣ =====>    N ○     × Gᵣ =====>    P ●     ● G
       / \                  / \                    / \   / \
   Pₗ ×   ○ N            P ●   × Nᵣ               ×   ×  ×  ×
         / \              / \                     Pₗ  Nₗ Nᵣ Gᵣ
     Nₗ ×   × Nᵣ       Pₗ ×  × Nₗ

(3) right_rotate(P); left_rotate(G);

    G ●                   G ●                      N ○
     / \       右回転      /   \       左回転        /   \
 Gₗ ×   ○ P    =====> Gₗ ×     ○ N    =====>    G ●     ● P
       / \                    / \                / \   / \
    N ○   × Pᵣ            Nₗ ×   ● P            ×   ×  ×  ×
     / \                        / \             Gₗ  Nₗ Nᵣ Pᵣ
 Nₗ ×   × Nᵣ                   Nᵣ  Pᵣ

(4) left_rotate(G);

    G ●                      P ○
     / \          左回転      /   \
 Gₗ ×   ○ P       =====>  G ●      ● N
       / \                 / \    / \
   Pₗ ×   ○ N             ×   ×  ×   ×
         / \              Gₗ  Pₗ Nₗ  Nᵣ
      Nₗ ×   × Nᵣ
*)

fun balance (TREE (BLACK, TREE (RED, TREE (RED, nl, nk, nr), pk, pr), gk, gr)) =
    TREE (RED, TREE (BLACK, nl, nk, nr), pk, TREE (BLACK, pr, gk, gr))
  | balance (TREE (BLACK, TREE (RED, pl, pk, TREE (RED, nl, nk, nr)), gk, gr)) =
    TREE (RED, TREE (BLACK, pl, pk, nl), nk, TREE (BLACK, nr, gk, gr))
  | balance (TREE (BLACK, gl, gk, TREE (RED, TREE (RED, nl, nk, nr), pk, pr))) =
    TREE (RED, TREE (BLACK, gl, gk, nl), nk, TREE (BLACK, nr, pk, pr))
  | balance (TREE (BLACK, gl, gk, TREE (RED, pl, pk, TREE (RED, nl, nk, nr)))) =
    TREE (RED, TREE (BLACK, gl, gk, pl), pk, TREE (BLACK, nl, nk, nr))
  | balance t = t;

fun balance_insert (key, LEAF) = TREE (RED, LEAF, key, LEAF)
  | balance_insert (key, TREE (c, l, k, r)) =
     if key < k then
	balance (TREE (c, (balance_insert (key, l)), k, r))
    else if key > k then
	balance (TREE (c, l, k, (balance_insert (key, r))))
    else
	TREE (c, l, k, r);

fun insert (key, t) =
    toBlack (balance_insert (key, t));

insert (8, insert (7, insert (6, insert (5, insert (4, insert (3, insert (2, insert (1, LEAF))))))));
