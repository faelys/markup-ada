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

------------------------------------------------------------------------------
-- Markup.Parsers is the parent of all language-specific parsers.           --
-- It contains a lexer-like implementation common to all parsers.           --
------------------------------------------------------------------------------

with Natools.String_Slices.Slice_Sets;

private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Ordered_Maps;
private with Natools.Indefinite_Holders;

package Markup.Parsers is

   type Tokenizer is interface;

   procedure Process
     (Object : in out Tokenizer;
      Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set) is abstract;
   --  Process the given piece of text to build a specific kind of token,
   --  and update Text.First if successful.



   type Unit_Lexer is tagged private;

   function Create (T : Tokenizer'Class) return Unit_Lexer;

   procedure Set_Tokenizer
     (Object : in out Unit_Lexer;
      T : in Tokenizer'Class);

   procedure Process
     (Object : in out Unit_Lexer;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set);



   type List_Lexer is tagged private;

   procedure Set_Fallback
     (Object : in out List_Lexer;
      T : in Tokenizer'Class);

   procedure Add_Tokenizer
     (Object : in out List_Lexer;
      T : in Tokenizer'Class);

   procedure Process
     (Object : in out List_Lexer;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set);



   type Table_Lexer is tagged private;

   procedure Add_Tokenizer
     (Object : in out Table_Lexer;
      Active_Char : in Character;
      T : in Tokenizer'Class);

   procedure Process
     (Object : in out Table_Lexer;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set;
      Current : in out Element_Callback'Class);

private

   package Tokenizer_Holders is new Natools.Indefinite_Holders
     (Tokenizer'Class);

   procedure Process
     (Holder : in out Tokenizer_Holders.Holder;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set);
   --  Call Process on the hold object, or raise Constraint_Error if empty


   type Unit_Lexer is tagged record
      Holder : Tokenizer_Holders.Holder;
   end record;



   package Tokenizer_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Tokenizer'Class);

   procedure Process
     (List : in out Tokenizer_Lists.List;
      Cursor : in Tokenizer_Lists.Cursor;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set);
   --  Call Process on the object designated by the cursor

   type List_Lexer is tagged record
      List : Tokenizer_Lists.List;
      Fallback : Tokenizer_Holders.Holder;
   end record;



   package Tokenizer_List_Maps is new Ada.Containers.Ordered_Maps
     (Character, Tokenizer_Lists.List, "<", Tokenizer_Lists."=");

   procedure Process
     (Map : in out Tokenizer_List_Maps.Map;
      Cursor : in Tokenizer_List_Maps.Cursor;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set);
   --  Call Process on each object of the list designated by Cursor until
   --  of them succeeds.

   type Table_Lexer is tagged record
      Map : Tokenizer_List_Maps.Map;
   end record;

end Markup.Parsers;
