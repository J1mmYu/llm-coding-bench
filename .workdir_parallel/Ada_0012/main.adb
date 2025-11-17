with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure RLE is
   Line : String := Get_Line;
   Count : Natural := 1;
begin
   if Line'Length = 0 then
      return;
   end if;
   
   for I in Line'First + 1 .. Line'Last loop
      if Line(I) = Line(I - 1) then
         Count := Count + 1;
      else
         Put(Line(I - 1) & Natural'Image(Count)(2 .. Natural'Image(Count)'Last));
         Count := 1;
      end if;
   end loop;
   
   Put(Line(Line'Last) & Natural'Image(Count)(2 .. Natural'Image(Count)'Last));
   New_Line;
end RLE;
