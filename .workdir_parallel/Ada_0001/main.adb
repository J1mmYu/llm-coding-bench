with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
procedure Main is
   N : Integer; S : Long_Long_Integer;
begin
   Get(N);
   S := Long_Long_Integer(N) * Long_Long_Integer(N + 1) / 2;
   Put(S, Width => 0);
   New_Line;
end Main;
