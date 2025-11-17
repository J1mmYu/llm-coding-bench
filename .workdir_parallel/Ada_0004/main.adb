with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Balanced_Brackets is
   package Char_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Character);
   
   Stack : Char_Vectors.Vector;
   Line  : String := Get_Line;
   Is_Balanced : Boolean := True;
   
   function Matches (Open, Close : Character) return Boolean is
   begin
      return (Open = '(' and Close = ')') or
             (Open = '[' and Close = ']') or
             (Open = '{' and Close = '}');
   end Matches;
   
begin
   for C of Line loop
      if C = '(' or C = '[' or C = '{' then
         Stack.Append(C);
      elsif C = ')' or C = ']' or C = '}' then
         if Stack.Is_Empty then
            Is_Balanced := False;
            exit;
         elsif Matches(Stack.Last_Element, C) then
            Stack.Delete_Last;
         else
            Is_Balanced := False;
            exit;
         end if;
      end if;
   end loop;
   
   if Is_Balanced and Stack.Is_Empty then
      Put_Line("YES");
   else
      Put_Line("NO");
   end if;
end Balanced_Brackets;
