and       break     do        else      elseif    end
false     for       function  goto      if        in
local     nil       not       or        repeat    return
then      true      until     while

+     -     *     /     %     ^     #
&     ~     |     <<    >>    //
==    ~=    <=    >=    <     >     =
(     )     {     }     [     ]     ::
;     :     ,     .     ..    ...

a = 'alo\n123"'
a = "alo\n123\""
a = '\97lo\10\04923"'
a = [[alo
123"]]
-- a = [==[
--[[ alo
123"]==]
]]
a = '\97lo\10\04923"\u{A91}'

a = { 3, 345, 0xff, 0xBEBADA }
a = { 3.0, 3.1416, 314.16e-2, 0.31416E1, 34e1 }
a = { 0x0.1E, 0xA23p-4, 0X1.921FB54442D18P+1 }