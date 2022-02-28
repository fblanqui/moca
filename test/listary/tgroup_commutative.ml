open Group_commutative
;;

open Gentest
;;

testing "Group_commutative (automatic) - seed: 605672937"
;;

testi 0
(try
 (let x0 = add [gen 27; gen 68; add [gen 24; gen 36; gen 43]] in
  let x1 = add [gen 27; gen 68; add [gen 24; gen 36; gen 43]] in
  let x2 = opp (add [gen 27; gen 63; gen 11]) in
  add [add [x0; x1]; x2] = add [x0; x1; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 1
(try
 (let x0 =
    add [gen 27; add [gen 24; gen 17; gen 47]; add [gen 20; gen 14; gen 49]] in
  let x1 = gen 13 in
  let x2 =
    add [gen 27; add [gen 24; gen 17; gen 47]; add [gen 20; gen 14; gen 49]] in
  add [add [x0; x1]; x2] = add [x0; x1; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 2
(try
 (let x0 =
    add [gen 27; add [gen 24; gen 17; gen 47]; add [gen 24; gen 36; gen 43]] in
  let x1 = opp (add [gen 61; zero; gen 50]) in
  let x2 = opp (add [gen 61; zero; gen 39]) in
  add [add [x0; x1]; x2] = add [x0; x1; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 3
(try
 (let x0 = opp (add [gen 61; zero; gen 39]) in
  let x1 = gen 15 in
  let x2 = add [add [gen 6; gen 6; gen 43]; opp (gen 40); gen 71] in
  add [add [x0; x1]; x2] = add [x0; x1; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 4
(try
 (let x0 = opp (gen 49) in
  let x1 =
    add [gen 27; add [gen 24; gen 17; gen 47]; add [gen 19; gen 36; gen 51]] in
  let x2 =
    add [gen 27; add [gen 24; gen 17; gen 47]; add [gen 19; gen 36; gen 51]] in
  add [add [x0; x1]; x2] = add [x0; x1; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 5
(try
 (let x0 =
    add [add [gen 94; gen 19; gen 108]; add [gen 36; gen 47; gen 42]; gen 28] in
  let x1 = opp (opp (gen 92)) in
  let x2 = add [gen 48; add [gen 36; gen 47; gen 42]; gen 25] in
  let x3 = gen 44 in
  let x4 = gen 95 in
  add [x0; add [x1; x2]; add [x3; x4]] = add [x0; x1; x2; x3; x4])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 6
(try
 (let x0 =
    add
      [add [gen 51; gen 101; gen 92]; add [gen 74; gen 79; gen 17]; add
       [gen 22; gen 31; gen 63]] in
  let x1 = gen 74 in
  let x2 = zero in
  let x3 = opp (opp (gen 122)) in
  let x4 = gen 74 in
  add [x0; add [x1; x2]; add [x3; x4]] = add [x0; x1; x2; x3; x4])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 7
(try
 (let x0 = opp (add [gen 98; gen 93; gen 123]) in
  let x1 = opp (opp (gen 100)) in
  let x2 = add [opp (gen 2); opp (gen 81); gen 28] in
  let x3 = gen 24 in
  let x4 =
    add [add [gen 91; gen 19; gen 114]; add [gen 36; zero; gen 94]; gen 3] in
  add [x0; add [x1; x2]; add [x3; x4]] = add [x0; x1; x2; x3; x4])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 8
(try
 (let x0 = add [opp (gen 54); add [gen 74; gen 58; gen 29]; opp (gen 120)] in
  let x1 = gen 95 in
  let x2 = add [gen 48; add [gen 74; gen 58; gen 29]; opp (gen 1)] in
  let x3 = gen 104 in
  let x4 = gen 35 in
  add [x0; add [x1; x2]; add [x3; x4]] = add [x0; x1; x2; x3; x4])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 9
(try
 (let x0 = gen 35 in
  let x1 =
    add [add [gen 94; gen 19; gen 108]; add [gen 36; gen 47; gen 42]; gen 28] in
  let x2 = add [gen 48; add [gen 36; zero; gen 94]; opp (gen 120)] in
  let x3 = gen 78 in
  let x4 = gen 44 in
  add [x0; add [x1; x2]; add [x3; x4]] = add [x0; x1; x2; x3; x4])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 10
(try
 (let x0 =
    add
      [add [gen 64; gen 27; gen 62]; add [gen 72; gen 28; gen 82]; opp (gen
       82)] in
  let x1 = gen 15 in
  let x2 = gen 51 in
  let x3 = opp (add [gen 19; zero; gen 59]) in
  add [add [x0; x1; x2]; x3] = add [x0; x1; x2; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 11
(try
 (let x0 = opp (opp (gen 41)) in
  let x1 = add [add [gen 64; zero; zero]; opp (gen 12); opp (gen 36)] in
  let x2 =
    add
      [add [gen 64; gen 27; gen 62]; add [gen 72; gen 28; gen 82]; opp (gen
       82)] in
  let x3 = gen 51 in
  add [add [x0; x1; x2]; x3] = add [x0; x1; x2; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 12
(try
 (let x0 = add [gen 61; opp (gen 14); gen 44] in
  let x1 = gen 52 in
  let x2 = add [gen 26; gen 26; opp (gen 36)] in
  let x3 = opp (add [gen 19; zero; gen 59]) in
  add [add [x0; x1; x2]; x3] = add [x0; x1; x2; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 13
(try
 (let x0 = add [add [gen 64; gen 27; gen 62]; gen 33; opp (gen 42)] in
  let x1 =
    add [add [gen 87; gen 83; gen 62]; add [gen 29; gen 32; gen 82]; gen 44] in
  let x2 =
    add
      [add [gen 64; gen 27; gen 62]; add [gen 72; gen 28; gen 82]; opp (gen
       82)] in
  let x3 = gen 90 in
  add [add [x0; x1; x2]; x3] = add [x0; x1; x2; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 14
(try
 (let x0 = opp (add [gen 27; gen 73; gen 89]) in
  let x1 = gen 15 in
  let x2 = add [gen 69; add [gen 20; gen 21; gen 92]; gen 41] in
  let x3 = add [opp (gen 66); add [gen 20; gen 21; gen 92]; opp (gen 36)] in
  add [add [x0; x1; x2]; x3] = add [x0; x1; x2; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 15
(try
 (let x0 = opp (add [gen 45; gen 49; gen 36]) in
  let x1 = add [gen 95; opp zero; add [gen 86; gen 33; gen 96]] in
  let x2 = add [gen 95; opp (gen 11); zero] in
  let x3 = add [gen 56; gen 21; gen 94] in
  add [x0; x1; x2; x3] = add [x1; x0; x3; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 16
(try
 (let x0 = gen 18 in
  let x1 = add [gen 95; opp (gen 11); zero] in
  let x2 = gen 96 in
  let x3 = gen 96 in
  add [x0; x1; x2; x3] = add [x1; x0; x3; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 17
(try
 (let x0 =
    add [add [gen 77; gen 77; gen 98]; add [gen 17; gen 34; gen 75]; gen 94] in
  let x1 = add [add [gen 77; gen 77; gen 98]; gen 85; opp (gen 21)] in
  let x2 = add [add [gen 77; gen 77; gen 98]; gen 97; opp (gen 30)] in
  let x3 =
    add [gen 52; add [gen 17; gen 34; gen 75]; add [gen 90; gen 88; gen 96]] in
  add [x0; x1; x2; x3] = add [x1; x0; x3; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 18
(try
 (let x0 = add [opp (gen 83); gen 33; opp (gen 21)] in
  let x1 = add [gen 95; opp (gen 11); zero] in
  let x2 = gen 60 in
  let x3 = gen 45 in
  add [x0; x1; x2; x3] = add [x1; x0; x3; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 19
(try
 (let x0 = gen 45 in
  let x1 = add [gen 95; add [gen 89; gen 88; gen 6]; gen 94] in
  let x2 = opp (gen 1) in
  let x3 = gen 54 in
  add [x0; x1; x2; x3] = add [x1; x0; x3; x2])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 20
(try
 (let x0 = gen 50 in
  let x1 = gen 3 in
  let x2 = opp (gen 20) in
  let x3 = gen 0 in
  add [x0; x1; x2; x3] = add [x3; x1; x2; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 21
(try
 (let x0 = gen 3 in
  let x1 = gen 50 in
  let x2 = gen 26 in
  let x3 = gen 83 in
  add [x0; x1; x2; x3] = add [x3; x1; x2; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 22
(try
 (let x0 = gen 50 in
  let x1 = gen 47 in
  let x2 = add [gen 19; gen 57; add [gen 74; gen 77; gen 97]] in
  let x3 = opp (gen 99) in
  add [x0; x1; x2; x3] = add [x3; x1; x2; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 23
(try
 (let x0 = add [gen 78; add [gen 76; gen 69; gen 99]; opp (gen 3)] in
  let x1 = gen 7 in
  let x2 = gen 2 in
  let x3 = add [gen 62; gen 95; add [gen 8; gen 93; gen 59]] in
  add [x0; x1; x2; x3] = add [x3; x1; x2; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 24
(try
 (let x0 = add [gen 81; opp (gen 74); opp (gen 89)] in
  let x1 = add [gen 81; opp (gen 74); opp (gen 89)] in
  let x2 = gen 50 in
  let x3 =
    add [add [gen 53; gen 51; gen 74]; add [gen 83; gen 0; gen 58]; gen 28] in
  add [x0; x1; x2; x3] = add [x3; x1; x2; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 25
(try
 (let x0 = zero in
  let x1 = add [opp (gen 54); gen 65; add [gen 71; gen 63; zero]] in
  let x2 = add [opp (gen 54); gen 65; add [gen 71; gen 63; zero]] in
  let x3 = add [gen 50; opp (gen 76); add [gen 58; gen 14; gen 89]] in
  add [x0; x1; x2; x3] = add [x3; x2; x1; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 26
(try
 (let x0 = add [zero; opp (gen 44); gen 88] in
  let x1 = add [add [gen 41; gen 48; gen 90]; opp (gen 44); opp (gen 64)] in
  let x2 = gen 85 in
  let x3 = gen 90 in
  add [x0; x1; x2; x3] = add [x3; x2; x1; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 27
(try
 (let x0 = gen 34 in
  let x1 = opp (gen 65) in
  let x2 = opp (gen 65) in
  let x3 = gen 2 in
  add [x0; x1; x2; x3] = add [x3; x2; x1; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 28
(try
 (let x0 = gen 34 in
  let x1 = add [add [gen 77; gen 40; gen 63]; opp (gen 44); opp (gen 5)] in
  let x2 = gen 85 in
  let x3 = add [add [gen 50; gen 48; gen 30]; gen 21; gen 28] in
  add [x0; x1; x2; x3] = add [x3; x2; x1; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 29
(try
 (let x0 = add [add [gen 50; gen 48; gen 30]; opp (gen 44); gen 28] in
  let x1 =
    add [add [gen 77; gen 40; gen 63]; add [gen 75; gen 39; gen 66]; gen 27] in
  let x2 = gen 84 in
  let x3 = gen 85 in
  add [x0; x1; x2; x3] = add [x3; x2; x1; x0])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 30
(try
 (let x0 = opp (opp (gen 7)) in
  let x1 = gen 61 in
  let x2 = opp (add [gen 3; gen 39; gen 79]) in
  let x3 = gen 57 in
  add [x0; x1; x2; x3] = add [x2; x1; x0; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 31
(try
 (let x0 =
    add
      [add [gen 61; gen 15; gen 96]; add [gen 7; gen 13; gen 53]; add
       [gen 57; zero; gen 99]] in
  let x1 = gen 50 in
  let x2 = add [opp (gen 12); gen 95; gen 4] in
  let x3 =
    add [add [gen 72; gen 15; gen 85]; add [gen 51; gen 71; gen 3]; gen 4] in
  add [x0; x1; x2; x3] = add [x2; x1; x0; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 32
(try
 (let x0 =
    add
      [add [gen 59; gen 15; gen 6]; opp (gen 84); add
       [gen 57; gen 51; gen 64]] in
  let x1 = gen 61 in
  let x2 = add [add [gen 61; gen 15; gen 96]; opp (gen 84); opp (gen 48)] in
  let x3 = add [opp (gen 78); gen 55; opp (gen 38)] in
  add [x0; x1; x2; x3] = add [x2; x1; x0; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 33
(try
 (let x0 = gen 50 in
  let x1 = opp (gen 21) in
  let x2 = gen 92 in
  let x3 = opp (gen 21) in
  add [x0; x1; x2; x3] = add [x2; x1; x0; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 34
(try
 (let x0 = opp (opp (gen 11)) in
  let x1 =
    add
      [add [gen 61; gen 15; gen 96]; add [gen 7; gen 13; gen 53]; add
       [gen 57; zero; gen 99]] in
  let x2 = add [opp (gen 12); opp (gen 48); opp (gen 48)] in
  let x3 =
    add
      [add [gen 72; gen 6; gen 85]; add [gen 7; gen 13; gen 53]; add
       [gen 57; zero; gen 99]] in
  add [x0; x1; x2; x3] = add [x2; x1; x0; x3])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 35
(try
 (let x0 = add [gen 37; opp (gen 4); add [zero; gen 18; gen 29]] in
  let x1 =
    add [add [gen 3; gen 41; gen 27]; gen 17; add [zero; gen 18; gen 29]] in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 36
(try
 (let x0 = add [opp (gen 8); opp (gen 4); opp (gen 49)] in
  let x1 = opp zero in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 37
(try
 (let x0 = gen 48 in
  let x1 =
    add [add [gen 3; gen 41; gen 27]; gen 17; add [zero; gen 18; gen 29]] in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 38
(try
 (let x0 = opp (add [gen 12; gen 12; gen 32]) in
  let x1 = zero in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 39
(try
 (let x0 =
    add [add [gen 40; gen 34; gen 27]; add [gen 16; gen 37; gen 18]; zero] in
  let x1 = gen 32 in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 40
(try
 (let x0 =
    add [add [gen 9; gen 18; gen 0]; add [gen 6; gen 1; gen 10]; gen 0] in
  add [zero; x0] = x0)
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 41
(try
 (let x0 =
    add
      [add [gen 9; gen 18; zero]; add [gen 7; gen 1; gen 10]; add
       [gen 16; gen 24; zero]] in
  add [zero; x0] = x0)
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 42
(try (let x0 = gen 17 in
      add [zero; x0] = x0) with
   | Failure "Division by Absorbent" -> true)
;;

testi 43
(try
 (let x0 =
    add [add [gen 9; gen 18; gen 0]; add [gen 6; gen 1; gen 10]; gen 0] in
  add [zero; x0] = x0)
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 44
(try (let x0 = gen 20 in
      add [zero; x0] = x0) with
   | Failure "Division by Absorbent" -> true)
;;

testi 45
(try (let x0 = gen 4 in
      add [x0; zero; zero] = add [x0; zero]) with
   | Failure "Division by Absorbent" -> true)
;;

testi 46
(try
 (let x0 = opp (add [zero; gen 8; gen 7]) in
  add [x0; zero; zero] = add [x0; zero])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 47
(try
 (let x0 =
    add [add [zero; gen 23; gen 21]; opp zero; add [zero; gen 8; gen 19]] in
  add [x0; zero; zero] = add [x0; zero])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 48
(try (let x0 = opp (opp (gen 10)) in
      add [x0; zero; zero] = add [x0; zero])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 49
(try
 (let x0 =
    add [add [zero; gen 23; gen 21]; opp zero; add [zero; gen 8; gen 19]] in
  add [x0; zero; zero] = add [x0; zero])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 50
(try
 (let x0 = gen 9 in
  let x1 = add [add [gen 36; gen 27; gen 43]; gen 28; zero] in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 51
(try
 (let x0 = gen 15 in
  let x1 = add [opp (gen 1); add [gen 6; gen 13; gen 48]; opp (gen 23)] in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 52
(try
 (let x0 = gen 9 in
  let x1 = opp zero in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 53
(try
 (let x0 = opp (gen 34) in
  let x1 = gen 18 in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 54
(try
 (let x0 = opp (opp (gen 16)) in
  let x1 = opp (add [gen 48; gen 27; gen 19]) in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 55
(try
 (let x0 =
    add [add [gen 22; gen 49; gen 21]; add [gen 22; gen 1; gen 24]; zero] in
  let x1 = opp (opp (gen 29)) in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 56
(try (let x0 = gen 36 in
      let x1 = gen 33 in
      add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 57
(try
 (let x0 = opp (opp (gen 38)) in
  let x1 = gen 3 in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 58
(try
 (let x0 =
    add [add [gen 22; gen 49; gen 21]; add [gen 22; gen 1; gen 24]; zero] in
  let x1 = gen 36 in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 59
(try
 (let x0 = gen 5 in
  let x1 = opp (gen 19) in
  add [x0; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 60
(try (let x0 = opp (opp (gen 6)) in
      add [x0; zero] = x0) with
   | Failure "Division by Absorbent" -> true)
;;

testi 61
(try (let x0 = zero in
      add [x0; zero] = x0) with
   | Failure "Division by Absorbent" -> true)
;;

testi 62
(try (let x0 = add [opp zero; gen 9; gen 16] in
      add [x0; zero] = x0) with
   | Failure "Division by Absorbent" -> true)
;;

testi 63
(try (let x0 = gen 2 in
      add [x0; zero] = x0) with
   | Failure "Division by Absorbent" -> true)
;;

testi 64
(try (let x0 = gen 10 in
      add [x0; zero] = x0) with
   | Failure "Division by Absorbent" -> true)
;;

testi 65
(try (let x1 = gen 8 in
      add [zero; zero; x1] = add [zero; x1]) with
   | Failure "Division by Absorbent" -> true)
;;

testi 66
(try (let x1 = gen 8 in
      add [zero; zero; x1] = add [zero; x1]) with
   | Failure "Division by Absorbent" -> true)
;;

testi 67
(try
 (let x1 = opp (add [gen 7; gen 22; gen 5]) in
  add [zero; zero; x1] = add [zero; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 68
(try
 (let x1 = add [opp (gen 4); zero; gen 21] in
  add [zero; zero; x1] = add [zero; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 69
(try (let x1 = gen 2 in
      add [zero; zero; x1] = add [zero; x1]) with
   | Failure "Division by Absorbent" -> true)
;;

testi 70
(try
 (let x0 = gen 28 in
  let x1 = opp (add [gen 16; gen 6; gen 15]) in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 71
(try
 (let x0 = add [add [gen 34; gen 17; gen 19]; opp (gen 43); gen 6] in
  let x1 = opp (opp (gen 45)) in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 72
(try
 (let x0 = opp (opp (gen 48)) in
  let x1 = opp (add [gen 16; gen 6; gen 15]) in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 73
(try
 (let x0 = gen 44 in
  let x1 = gen 23 in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;

testi 74
(try
 (let x0 = gen 49 in
  let x1 = add [gen 22; add [gen 49; gen 36; gen 19]; gen 41] in
  add [x0; zero; zero; x1] = add [x0; x1])
 with
   | Failure "Division by Absorbent" -> true)
;;
