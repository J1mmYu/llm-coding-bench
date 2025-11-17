with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure CSV_Parser is
   Line : String (1 .. 10000);
   Last : Natural;
   Field_Count : Natural := 0;
   I : Positive := 1;
   In_Quotes : Boolean := False;
   Ch : Character;
begin
   Ada.Text_IO.Get_Line (Line, Last);
   
   if Last = 0 then
      Ada.Text_IO.Put_Line ("0");
      return;
   end if;
   
   Field_Count := 1;
   
   while I <= Last loop
      Ch := Line (I);
      
      if In_Quotes then
         if Ch = '"' then
            if I < Last and then Line (I + 1) = '"' then
               -- Escaped quote ""
               I := I + 1;
            else
               -- End of quoted field
               In_Quotes := False;
            end if;
         end if;
      else
         if Ch = '"' then
            In_Quotes := True;
         elsif Ch = ',' then
            Field_Count := Field_Count + 1;
         end if;
      end if;
      
      I := I + 1;
   end loop;
   
   Ada.Text_IO.Put_Line (Natural'Image (Field_Count));
end CSV_Parser;
