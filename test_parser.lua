a = 7;
a, b = 2, 3
c, t = "ssss", {f = false}
ff = function(a, b, c)
    a1 = 4
    b2 = a1;
    return (a + b + c) / (a1 + b2)
end

gggg().f = 5

ff(1, 2, 3);
table:method("print")

::label_name::
break
goto label_name

;;;;
return  nil, true, false, 44, 44.5, 'quote', "double quote",
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
        get().obj.field:func(1, 2, "string", false),
        {
            1,
            "sss";
            false,
            [2] = "s";
            ["d"] = 4,
            field = value;
            func = function() return 4*5 end,
            mm = function()
                do a = 567; b = 666 end
                while true do print("while-loop") end
                repeat print("repeat-until-loop") until false
                if 1 + 1 == 2 then
                    print(1)
                elseif true and false or 1 + 2 > 1 then
                    print("aaa")
                elseif false then
                    print("bbb")
                else
                    print(56)
                end
                for i = 1, 10 do print(i) end
                for i = 1, 5, 2 do print(i) end
                for k, v in pairs(table) do print(k, v) end
                for k, v in pairs(table), t, nil do print(k, v) end
                function test_func(...)
                    print(...)
                end
                function table.test_func(a)
                    return a + 1
                end
                function table.obj:test_method()
                    return self.field
                end
            end
        },
        {}