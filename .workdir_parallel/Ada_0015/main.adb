with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Max_Distinct is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   
   function Count_Distinct(S : String) return Natural is
      Seen : array (Character) of Boolean := (others => False);
      Count : Natural := 0;
   begin
      for I in S'Range loop
         if not Seen(S(I)) then
            Seen(S(I)) := True;
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Distinct;
   
   Max_Line : Unbounded_String := Null_Unbounded_String;
   Max_Count : Natural := 0;
   Current_Line : Unbounded_String;
   
begin
   while not End_Of_File loop
      Current_Line := To_Unbounded_String(Get_Line);
      declare
         Current_Count : constant Natural := Count_Distinct(To_String(Current_Line));
      begin
         if Current_Count > Max_Count then
            Max_Count := Current_Count;
            Max_Line := Current_Line;
         end if;
      end;
   end loop;
   
   Put_Line(To_String(Max_Line));
end Max_Distinct;
