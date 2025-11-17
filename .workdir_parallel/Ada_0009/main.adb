with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;

procedure Main is
   Input : String := Get_Line;
   Dot_Count : Natural := 0;
   Start_Pos : Positive := 1;
   Valid : Boolean := True;
   
   function Is_Valid_Octet (S : String) return Boolean is
      Num : Integer;
   begin
      if S'Length = 0 or S'Length > 3 then
         return False;
      end if;
      
      -- Check all characters are digits
      for I in S'Range loop
         if S(I) < '0' or S(I) > '9' then
            return False;
         end if;
      end loop;
      
      -- Check for leading zeros (except "0" itself)
      if S'Length > 1 and then S(S'First) = '0' then
         return False;
      end if;
      
      -- Check range 0-255
      Num := Integer'Value(S);
      return Num >= 0 and Num <= 255;
   exception
      when others =>
         return False;
   end Is_Valid_Octet;
   
begin
   if Input'Length = 0 then
      Put_Line("0");
      return;
   end if;
   
   -- Count dots and validate octets
   for I in Input'Range loop
      if Input(I) = '.' then
         Dot_Count := Dot_Count + 1;
         if Dot_Count > 3 then
            Valid := False;
            exit;
         end if;
         
         if not Is_Valid_Octet(Input(Start_Pos .. I - 1)) then
            Valid := False;
            exit;
         end if;
         
         Start_Pos := I + 1;
      end if;
   end loop;
   
   -- Check last octet and dot count
   if Valid and Dot_Count = 3 then
      Valid := Is_Valid_Octet(Input(Start_Pos .. Input'Last));
   else
      Valid := False;
   end if;
   
   if Valid then
      Put_Line("1");
   else
      Put_Line("0");
   end if;
end Main;
