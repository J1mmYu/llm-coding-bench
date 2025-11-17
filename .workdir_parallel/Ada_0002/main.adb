with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
procedure Main is
   Ch  : Character;
   Buf : String(1..1000);
   Len : Natural := 0;
begin
   loop
      exit when End_Of_File;
      Get(Ch);
      exit when Ch = LF;
      Len := Len + 1;
      Buf(Len) := Ch;
   end loop;
   for I in reverse 1..Len loop
      Put(Buf(I));
   end loop;
   New_Line;
end Main;
