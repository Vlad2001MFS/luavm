;;;;
return nil, true, false, 44, 44.5, 'quote', "double quote",
       -666, not false, #56, ~true,
       1 + (2*3 - 4 / 2 % 6)^2,
       1..2 << 3 >> 5 & 6 ~ 7 | 8,
       1 < 2, 3 > 4, 5 <= 6, 7 >= 8, 9 ~= 10, 11 == 12,
       1 < 2 and 3 > 2 or 1 ~= 2, ...,
       function(a, b) return 1..2 << 3 >> 5 & 6 ~ 7 | 8 end,
       function(...) return 1 < 2, 3 > 4, 5 <= 6, 7 >= 8, 9 ~= 10, 11 == 12 end,
       function(a, b, ...) return 1 < 2 and 3 > 2 or 1 ~= 2, ... end,
       (1 + 2*3),
       abc,
       (table)[get_idx() + 1][i].obj.field,
       get().obj.field:func(1, 2, "string", false)