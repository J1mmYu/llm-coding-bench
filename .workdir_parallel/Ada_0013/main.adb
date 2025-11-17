with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Big_Int_Sum is
   function Add_Big_Integers(A, B : String) return String is
      Len_A : constant Natural := A'Length;
      Len_B : constant Natural := B'Length;
      Max_Len : constant Natural := Natural'Max(Len_A, Len_B);
      Result : String(1 .. Max_Len + 1) := (others => '0');
      Carry : Natural := 0;
      Digit_A, Digit_B, Sum : Natural;
      Pos : Natural := Max_Len + 1;
   begin
      for I in 0 .. Max_Len - 1 loop
         if I < Len_A then
            Digit_A := Character'Pos(A(Len_A - I)) - Character'Pos('0');
         else
            Digit_A := 0;
         end if;
         
         if I < Len_B then
            Digit_B := Character'Pos(B(Len_B - I)) - Character'Pos('0');
         else
            Digit_B := 0;
         end if;
         
         Sum := Digit_A + Digit_B + Carry;
         Result(Pos) := Character'Val(Character'Pos('0') + (Sum mod 10));
         Carry := Sum / 10;
         Pos := Pos - 1;
      end loop;
      
      if Carry > 0 then
         Result(1) := Character'Val(Character'Pos('0') + Carry);
         return Result;
      else
         return Result(2 .. Result'Last);
      end if;
   end Add_Big_Integers;
   
   A_Str, B_Str : Unbounded_String;
begin
   A_Str := To_Unbounded_String(Get_Line);
   B_Str := To_Unbounded_String(Get_Line);
   Put_Line(Add_Big_Integers(To_String(A_Str), To_String(B_Str)));
end Big_Int_Sum;
