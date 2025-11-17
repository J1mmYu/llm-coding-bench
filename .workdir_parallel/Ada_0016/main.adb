with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   Input : String := Get_Line;
   Pos : Integer := 1;

   function Parse_Expression return Integer;

   function Skip_Spaces is
   begin
      while Pos <= Input'Length and then Input(Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;
   end Skip_Spaces;

   function Parse_Number return Integer is
      Result : Integer := 0;
   begin
      Skip_Spaces;
      while Pos <= Input'Length and then Input(Pos) in '0'..'9' loop
         Result := Result * 10 + (Character'Pos(Input(Pos)) - Character'Pos('0'));
         Pos := Pos + 1;
      end loop;
      return Result;
   end Parse_Number;

   function Parse_Factor return Integer is
      Result : Integer;
   begin
      Skip_Spaces;
      if Pos <= Input'Length and then Input(Pos) = '(' then
         Pos := Pos + 1;
         Result := Parse_Expression;
         Skip_Spaces;
         if Pos <= Input'Length and then Input(Pos) = ')' then
            Pos := Pos + 1;
         end if;
         return Result;
      else
         return Parse_Number;
      end if;
   end Parse_Factor;

   function Parse_Term return Integer is
      Result : Integer := Parse_Factor;
      Op : Character;
   begin
      loop
         Skip_Spaces;
         exit when Pos > Input'Length or else (Input(Pos) /= '*' and Input(Pos) /= '/');
         Op := Input(Pos);
         Pos := Pos + 1;
         if Op = '*' then
            Result := Result * Parse_Factor;
         else
            Result := Result / Parse_Factor;
         end if;
      end loop;
      return Result;
   end Parse_Term;

   function Parse_Expression return Integer is
      Result : Integer := Parse_Term;
      Op : Character;
   begin
      loop
         Skip_Spaces;
         exit when Pos > Input'Length or else (Input(Pos) /= '+' and Input(Pos) /= '-');
         Op := Input(Pos);
         Pos := Pos + 1;
         if Op = '+' then
            Result := Result + Parse_Term;
         else
            Result := Result - Parse_Term;
         end if;
      end loop;
      return Result;
   end Parse_Expression;

begin
   Put_Line(Integer'Image(Parse_Expression));
end Main;
