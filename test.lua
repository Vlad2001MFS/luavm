a = 'alo\n123"\u{A91}'
a = "alo\n123\""
a = '\97lo\10\04923"'
a = '\x61lo\10\04923"'
-- a = [[alo
-- 123"]]
-- a = [==[
-- alo
-- 123"]==]

-- io.write
-- (--[[ my first-- hjglit769780y906 --[[ my --[[ my-- hjglit769780y906 first program in Lua --]]first program in Lua --]]program in Lua --]]
--    "Hello \"world, \\from ",_VERSION,"!\n"
--    --ghkghkfkgjfkjffkjgj -- hjglit769780y906
-- )

-- test = {
--     3,   345,   0xff,   0xBEBADA,
--     -1.0, 3.0,     3.1416,     314.16e-2,     0.31416E1,     34e1,
--     -0x0.1E, 0x0.1E,  0xA23p-4,   0X1.921FB54442D18P+1
-- }

-- b = (4*5 - 6*(- 7)) / 2

-- for k, v in pairs(test) do
--     print(v)
-- end




-- --[[
--    table.binsearch( table, value [, compval [, reversed] ] )
   
--    Searches the table through BinarySearch for the given value.
--    If the  value is found:
--       it returns a table holding all the mathing indices (e.g. { startindice,endindice } )
--       endindice may be the same as startindice if only one matching indice was found
--    If compval is given:
--       then it must be a function that takes one value and returns a second value2,
--       to be compared with the input value, e.g.:
--       compvalue = function( value ) return value[1] end
--    If reversed is set to true:
--       then the search assumes that the table is sorted in reverse order (largest value at position 1)
--       note when reversed is given compval must be given as well, it can be nil/_ in this case
--    Return value:
--       on success: a table holding matching indices (e.g. { startindice,endindice } )
--       on failure: nil
-- ]]--
-- do
--     -- Avoid heap allocs for performance
--     local default_fcompval = function( value ) return value end
--     local fcompf = function( a,b ) return a < b end
--     local fcompr = function( a,b ) return a > b end
--     function table.binsearch( tbl,value,fcompval,reversed )
--        -- Initialise functions
--        local fcompval = fcompval or default_fcompval
--        local fcomp = reversed and fcompr or fcompf
--        --  Initialise numbers
--        local iStart,iEnd,iMid = 1,#tbl,0
--        -- Binary Search
--        while iStart <= iEnd do
--           -- calculate middle
--           iMid = math.floor( (iStart+iEnd)/2 )
--           -- get compare value
--           local value2 = fcompval( tbl[iMid] )
--           -- get all values that match
--           if value == value2 then
--              local tfound,num = { iMid,iMid },iMid - 1
--              while value == fcompval( tbl[num] ) do -- ERROR: this may cause fail in fcompval if num is out of range and tbl[num] is nil
--                 tfound[1],num = num,num - 1
--              end
--              num = iMid + 1
--              while value == fcompval( tbl[num] ) do -- ERROR: this may cause fail in fcompval if num is out of range and tbl[num] is nil
--                 tfound[2],num = num,num + 1
--              end
--              return tfound
--           -- keep searching
--           elseif fcomp( value,value2 ) then
--              iEnd = iMid - 1
--           else
--              iStart = iMid + 1
--           end
--        end
--     end
--  end
--  -- CHILLCODE™




--  -- test: table size 0
-- t = {}
-- local v = table.binsearch(t, 5); assert(v == nil)

-- -- test: table size 1
-- t = {5}
-- local v = table.binsearch(t, 4); assert(v == nil)
-- local v = table.binsearch(t, 5); assert(v[1] == 1)
-- local v = table.binsearch(t, 6); assert(v == nil)

-- -- test: table size 2
-- t = {4,6}
-- local v = table.binsearch(t, 3); assert(v == nil)
-- local v = table.binsearch(t, 4); assert(v[1] == 1)
-- local v = table.binsearch(t, 5); assert(v == nil)
-- local v = table.binsearch(t, 6); assert(v[1] == 2)
-- local v = table.binsearch(t, 7); assert(v == nil)

-- -- test: typical, with duplicate
-- t = {0,2,4,4,6,8,10}
-- local v = table.binsearch(t, 0); assert(v[1] == 1)
-- local v = table.binsearch(t, 10); assert(v[1] == 7)
-- local v = table.binsearch(t, 4); assert(v[1] == 3 and v[2] == 4)
-- local v = table.binsearch(t, 5); assert(v == nil)
-- local v = table.binsearch(t, 11); assert(v == nil)
-- local v = table.binsearch(t, -1); assert(v == nil)

-- -- test: identical
-- t = {1,1,1,1,1,1,1,1,1,1}
-- local v = table.binsearch(t, 1); assert(v[1] == 1 and v[2] == 10)

-- -- test: fcomp
-- t = {10,52,34,44,86,38}
-- local v = table.binsearch(t, 6, function(v) return v % 10 end); assert(v[1] == 5)
-- local v = table.binsearch(t, 4, function(v) return v % 10 end); assert(v[1] == 3 and v[2] == 4)

-- -- test: reverse
-- t = {10,8,6,4,4,2,0}
-- local v = table.binsearch(t, 6,_,1); assert(v[1] == 3)
-- local v = table.binsearch(t, 4,_,1); assert(v[1] == 4 and v[2] == 5)

-- print "DONE"