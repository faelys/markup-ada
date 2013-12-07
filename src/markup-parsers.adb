------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Strings.Maps;

package body Markup.Parsers is

   package Maps renames Ada.Strings.Maps;
   package Slices renames Natools.String_Slices;


   ---------------------------
   -- Container subprograms --
   ---------------------------

   procedure Process
     (Holder : in out Tokenizer_Holders.Holder;
      Text : in out Slices.Slice_Sets.Slice_Set)
   is
      procedure Dispatch_Process (T : in out Tokenizer'Class);

      procedure Dispatch_Process (T : in out Tokenizer'Class) is
      begin
         T.Process (Text);
      end Dispatch_Process;
   begin
      Holder.Update_Element (Dispatch_Process'Access);
   end Process;


   procedure Process
     (List : in out Tokenizer_Lists.List;
      Cursor : in Tokenizer_Lists.Cursor;
      Text : in out Slices.Slice_Sets.Slice_Set)
   is
      procedure Dispatch_Process (T : in out Tokenizer'Class);

      procedure Dispatch_Process (T : in out Tokenizer'Class) is
      begin
         T.Process (Text);
      end Dispatch_Process;
   begin
      List.Update_Element (Cursor, Dispatch_Process'Access);
   end Process;


   procedure Process
     (Map : in out Tokenizer_List_Maps.Map;
      Cursor : in Tokenizer_List_Maps.Cursor;
      Text : in out Slices.Slice_Sets.Slice_Set)
   is
      procedure Dispatch_Process
        (Key : in Character;
         List : in out Tokenizer_Lists.List);

      procedure Dispatch_Process
        (Key : in Character;
         List : in out Tokenizer_Lists.List)
      is
         pragma Unreferenced (Key);
         use type Slices.String_Range;

         Cursor : Tokenizer_Lists.Cursor := List.First;
         Current, Previous : Positive;
      begin
         Current := Text.First;
         loop
            exit when not Tokenizer_Lists.Has_Element (Cursor);
            Previous := Current;
            Process (List, Cursor, Text);
            exit when Text.Is_Empty;
            Current := Text.First;
            exit when Current /= Previous;
            Tokenizer_Lists.Next (Cursor);
         end loop;
      end Dispatch_Process;
   begin
      Map.Update_Element (Cursor, Dispatch_Process'Access);
   end Process;



   ----------------
   -- Unit_Lexer --
   ----------------

   function Create (T : Tokenizer'Class) return Unit_Lexer is
   begin
      return Unit_Lexer'(Holder => Tokenizer_Holders.To_Holder (T));
   end Create;


   procedure Set_Tokenizer
     (Object : in out Unit_Lexer;
      T : in Tokenizer'Class) is
   begin
      Object.Holder.Replace_Element (T);
   end Set_Tokenizer;


   procedure Process
     (Object : in out Unit_Lexer;
      Text : in out Slices.Slice_Sets.Slice_Set)
   is
      use type Slices.String_Range;

      Previous_Index : Positive := Text.First;
      Current_Index : Positive;
   begin
      if Text.Is_Empty then
         return;
      end if;

      loop
         Process (Object.Holder, Text);
         exit when Text.Is_Empty;
         Current_Index := Text.First;
         exit when Current_Index = Previous_Index;
         Previous_Index := Current_Index;
      end loop;
   end Process;



   ----------------
   -- List_Lexer --
   ----------------

   procedure Set_Fallback
     (Object : in out List_Lexer;
      T : in Tokenizer'Class) is
   begin
      Object.Fallback.Replace_Element (T);
   end Set_Fallback;


   procedure Add_Tokenizer
     (Object : in out List_Lexer;
      T : in Tokenizer'Class) is
   begin
      Object.List.Append (T);
   end Add_Tokenizer;


   procedure Add_Tokenizer_After
     (Object : in out List_Lexer;
      T : in Tokenizer'Class;
      Relative : not null access function
        (T : Tokenizer'Class) return Boolean)
   is
      procedure Check_Object (T : in Tokenizer'Class);

      Is_Relative : Boolean := False;

      procedure Check_Object (T : in Tokenizer'Class) is
      begin
         Is_Relative := Relative (T);
      end Check_Object;

      Current : Tokenizer_Lists.Cursor := Object.List.Last;
   begin
      while Tokenizer_Lists.Has_Element (Current) loop
         Tokenizer_Lists.Query_Element (Current, Check_Object'Access);
         exit when Is_Relative;
         Tokenizer_Lists.Previous (Current);
      end loop;

      if Is_Relative then
         Tokenizer_Lists.Next (Current);
         if Tokenizer_Lists.Has_Element (Current) then
            Object.List.Insert (Current, T);
         else
            Object.List.Append (T);
         end if;
      else
         Object.List.Append (T);
      end if;
   end Add_Tokenizer_After;


   procedure Add_Tokenizer_Before
     (Object : in out List_Lexer;
      T : in Tokenizer'Class;
      Relative : not null access function
        (T : Tokenizer'Class) return Boolean)
   is
      procedure Check_Object (T : in Tokenizer'Class);

      Is_Relative : Boolean := False;

      procedure Check_Object (T : in Tokenizer'Class) is
      begin
         Is_Relative := Relative (T);
      end Check_Object;

      Current : Tokenizer_Lists.Cursor := Object.List.First;
   begin
      while Tokenizer_Lists.Has_Element (Current) loop
         Tokenizer_Lists.Query_Element (Current, Check_Object'Access);
         exit when Is_Relative;
         Tokenizer_Lists.Next (Current);
      end loop;

      if Is_Relative then
         Object.List.Insert (Current, T);
      else
         Object.List.Append (T);
      end if;
   end Add_Tokenizer_Before;


   procedure Process
     (Object : in out List_Lexer;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set)
   is
      use type Slices.String_Range;

      Previous_Index : Positive;
      Current_Index : Positive;
      Cursor : Tokenizer_Lists.Cursor;
   begin
      if Text.Is_Empty then
         return;
      end if;

      Previous_Index := Text.First;
      Cursor := Object.List.First;

      loop
         if Tokenizer_Lists.Has_Element (Cursor) then
            Process (Object.List, Cursor, Text);
            exit when Text.Is_Empty;
            Current_Index := Text.First;

            if Current_Index = Previous_Index then
               Tokenizer_Lists.Next (Cursor);
            else
               Cursor := Object.List.First;
               Previous_Index := Current_Index;
            end if;
         else
            exit when Object.Fallback.Is_Empty;
            Process (Object.Fallback, Text);
            exit when Text.Is_Empty;
            Current_Index := Text.First;
            exit when Current_Index = Previous_Index;

            Cursor := Object.List.First;
            Previous_Index := Current_Index;
         end if;
      end loop;
   end Process;



   -----------------
   -- Table_Lexer --
   -----------------

   procedure Add_Tokenizer
     (Object : in out Table_Lexer;
      Active_Char : in Character;
      T : in Tokenizer'Class)
   is
      procedure Process_List
        (Key : in Character;
         List : in out Tokenizer_Lists.List);

      Cursor : Tokenizer_List_Maps.Cursor;
      Inserted : Boolean;

      procedure Process_List
        (Key : in Character;
         List : in out Tokenizer_Lists.List) is
      begin
         pragma Assert (Key = Active_Char);
         pragma Assert
           ((Inserted and then List.Is_Empty)
            or else (not Inserted and then not List.Is_Empty));
         List.Append (T);
      end Process_List;
   begin
      Object.Map.Insert (Active_Char, Cursor, Inserted);
      Object.Map.Update_Element (Cursor, Process_List'Access);
   end Add_Tokenizer;


   procedure Process
     (Object : in out Table_Lexer;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set;
      Current : in out Element_Callback'Class)
   is
      use type Maps.Character_Set;

      Cursor : Tokenizer_List_Maps.Cursor;
      Active_Set : Maps.Character_Set := Maps.Null_Set;
      Position, Text_First : Positive;
      N : Natural;
      C : Character;
   begin
      if Text.Is_Empty then
         return;
      end if;

      --  Build the list of active characters

      Cursor := Object.Map.First;
      while Tokenizer_List_Maps.Has_Element (Cursor) loop
         Active_Set := Active_Set
           or Maps.To_Set (Tokenizer_List_Maps.Key (Cursor));
         Tokenizer_List_Maps.Next (Cursor);
      end loop;

      --  Process input text

      Text_First := Text.First;
      Position := Text_First;

      loop
         --  Feed inactive character slice to current element

         N := Text.Index
           (Set    => Active_Set,
            From   => Position,
            Test   => Ada.Strings.Inside,
            Going  => Ada.Strings.Forward);

         if N = 0 then
            Current.Append (Text.To_String);
            Text.Clear;
            return;
         end if;

         if N - 1 >= Text_First then
            declare
               Inactive_Range : constant Slices.String_Range
                 := Slices.To_Range (Text_First, N - 1);
            begin
               Current.Append (Text.To_String (Inactive_Range));
               Text.Exclude_Slice (Inactive_Range);
            end;
         end if;

         Position := N;
         pragma Assert (Text.First = Position);
         C := Text.Element (Position);
         pragma Assert (Maps.Is_In (C, Active_Set));

         --  Hand over the text to tokenizers

         Cursor := Object.Map.Find (C);
         pragma Assert (Tokenizer_List_Maps.Has_Element (Cursor));

         Process (Object.Map, Cursor, Text);
         exit when Text.Is_Empty;
         Text_First := Text.First;

         if Text_First = Position then
            Position := Position + 1;
         else
            pragma Assert (Text_First > Position);
            Position := Text_First;
         end if;
      end loop;
   end Process;

end Markup.Parsers;
