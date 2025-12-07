with HAT;

procedure Day07 is

   subtype VString is HAT.VString;

   type Feature_Enum is (Empty, Splitter);

   Columns : constant := 141;
   Rows    : constant := 142;

   subtype Col_Range is Positive range 1 .. Columns;
   subtype Row_Range is Positive range 1 .. Rows;

   Manifold : array (Row_Range, Col_Range) of Feature_Enum;

   type Position_Record is record
      Col : Col_Range;
      Row : Row_Range;
   end record;

   Start_Position : Position_Record;

   procedure Read_Input is
      Input : HAT.File_Type;
      Line  : VString;
   begin
      HAT.Open (Input, "input.txt");
      for Row in Row_Range loop
         HAT.Get_Line (Input, Line);
         for Col in Col_Range loop
            case HAT.Element (Line, Col) is
               when '^'    =>
                  Manifold (Row, Col) := Splitter;

               when 'S'    =>
                  Start_Position.Col := Col;
                  Start_Position.Row := Row;
                  Manifold (Row, Col) := Empty;

               when others =>
                  Manifold (Row, Col) := Empty;

            end case;
         end loop;
      end loop;
      HAT.Close (Input);
   end Read_Input;

   type Row_Set_Array is array (Col_Range) of Boolean;

   procedure Clear_Row_Set (Row_Set : out Row_Set_Array) is
   begin
      for Col in Col_Range loop
         Row_Set (Col) := False;
      end loop;
   end Clear_Row_Set;

   function Part_1 return Natural is
      Result     : Natural := 0;
      Curr, Next : Row_Set_Array;
   begin
      Clear_Row_Set (Curr);
      Curr (Start_Position.Col) := True;
      for Row in Row_Range'First + 1 .. Row_Range'Last loop
         Clear_Row_Set (Next);
         for Col in Col_Range loop
            if Curr (Col) then
               if Manifold (Row - 1, Col) = Empty then
                  Next (Col) := True;
               else
                  Next (Col + 1) := True;
                  Next (Col - 1) := True;
                  Result := Result + 1;
               end if;
            end if;
         end loop;
         Curr := Next;
      end loop;
      return Result;
   end Part_1;

   type Row_Count_Array is array (Col_Range) of Integer;

   procedure Clear_Row_Count (Row_Count : out Row_Count_Array) is
   begin
      for Col in Col_Range loop
         Row_Count (Col) := 0;
      end loop;
   end Clear_Row_Count;

   function Sum (Row_Count : Row_Count_Array) return Natural is
      Result : Natural := 0;
   begin
      for Col in Col_Range loop
         Result := Result + Row_Count (Col);
      end loop;
      return Result;
   end Sum;

   function Part_2 return Natural is
      Curr, Next : Row_Count_Array;
      Row        : Row_Range := Row_Range'First;
      Value      : Natural;
   begin
      Clear_Row_Count (Curr);
      Curr (Start_Position.Col) := 1;
      while Row < Row_Range'Last loop
         Clear_Row_Count (Next);
         for Col in Col_Range loop
            if Curr (Col) > 0 then
               Value := Curr (Col);
               if Manifold (Row, Col) = Empty then
                  Next (Col) := Next (Col) + Value;
               else
                  Next (Col - 1) := Next (Col - 1) + Value;
                  Next (Col + 1) := Next (Col + 1) + Value;
               end if;
            end if;
         end loop;
         Row := Row + 1;
         Curr := Next;
      end loop;
      return Sum (Curr);
   end Part_2;

begin
   Read_Input;
   HAT.Put ("Starting at ");
   HAT.Put (HAT.Image (Start_Position.Row));
   Hat.Put (" ");
   HAT.Put (HAT.Image (Start_Position.Col));
   HAT.New_Line;
   HAT.Put ("The beam splits ");
   HAT.Put (HAT.Image (Part_1));
   HAT.Put_Line (" times");
   HAT.Put ("A single particle ends up on ");
   HAT.Put (HAT.Image (Part_2));
   HAT.Put_Line (" timelines");
end Day07;
