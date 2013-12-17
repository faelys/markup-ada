# Overview #

`Markdown-ada` is meant to be a flexible library for parsing input texts
formatted in a lightweight markup language, and rendering them in various
output formats.

For higher flexibility, the renderer is connected to the parser by the
library client, element by element. This allows each language element to be
individually enabled or disabled, or rendered differently depending on
client's wishes.

Currently the only supported input format is
[Markdown](http://daringfireball.net/projects/markdown/), optionally with
[Discount](http://www.pell.portland.or.us/~orc/Code/discount/) extensions.
Others are planned to be added in the future.

Similarly, currently only (X)HTML output is supported, though other formats
will eventually appear, or clients can roll their own.

# Basic usage #

The link between parser and rendered is based on an Ada 2005 interface
`Element_Callback`, that represent how the renderer deals with a particular
text element. Renderers provide objects that implement the interface,
usually through a client-visible function. Parsers are configured by
procedure requiring those objects, storing them in the parser internal
state.

For example, in the provided markdown-to-HTML translator, `markdown.adb`,
parser and renderer objects are created at the beginning of the main
procedure:

    Parser : Markup.Parsers.Markdown.Extensions.Extended_Parser;
    Renderer : Markup.Renderers.Instances.Html_Stream.Renderer_Ref;

Then they are associated with statements like the following:

    Parser.Atx_Header (Renderer.Header);

Here, `Renderer.Header` returns an object that implement `Element_Callback`
interface to render an HTML header (`h1` to `h6` tags), while
`Parser.Header` is a procedure that stored the callback in the parser
internal state, to be used when encountering an ATX-style markdown header.

Sometimes several renderer elements are required for the same input
language element, for example:

    Parser.List
      (Renderer.Ordered_List,
       Renderer.List_Item,
       Markup.Parsers.Markdown.Styles.Ordered);

A Markdown list is rendered as a single list element containing a sequence
of item elements. This is also an example of a parser procedure that
require an extra parameter, so that the parser knows that renderer elements
are to be used only for ordered lists.
