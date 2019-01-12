defmodule PandoraTest do
  use ExUnit.Case

  require Pandora, as: T
  require Pandora.Data, as: Data

  describe "prepend" do
    test "prepend into tag" do
      init = T.create_element("Value")
      text = T.create_text("hello")
      {:ok, result} = T.prepend(init, [], text)

      expect =
        T.create_element("Value", %{}, [
          T.create_text("hello")
        ])

      assert T.equal(result, expect)
    end

    test "nested prepend" do
      init =
        T.create_element("Call", %{}, [
          T.create_element("Operator"),
          T.create_element("Arg", %{}, [T.create_text("2")]),
          T.create_element("Arg", %{}, [T.create_text("2")])
        ])

      text = T.create_text("+")
      {:ok, result} = T.prepend(init, [el: "Operator"], text)

      expect =
        T.create_element("Call", %{}, [
          T.create_element("Operator", %{}, [text]),
          T.create_element("Arg", %{}, [T.create_text("2")]),
          T.create_element("Arg", %{}, [T.create_text("2")])
        ])

      assert T.equal(result, expect)
    end
  end

  describe "to_string" do
    test "<Hello>world</Hello>" do
      init =
        T.create_element("Hello", %{}, [
          T.create_text("world")
        ])

      expect = "<Hello>world</Hello>"
      assert T.to_string(init) == expect
    end

    test "<Hello/>" do
      init = T.create_element("Hello")
      expect = "<Hello/>"
      assert T.to_string(init) == expect
    end

    test "<Hello to=\"world\"/>" do
      init = T.create_element("Hello", %{ "to" => "world" })
      expect = "<Hello to=\"world\"/>"
      assert T.to_string(init) == expect
    end

    test "<Hello><World>!</World></Hello>" do
      text = T.create_text("!")
      world = T.create_element("World", %{}, [text])
      hello = T.create_element("Hello", %{}, [world])
      expect = "<Hello><World>!</World></Hello>"
      assert T.to_string(hello) == expect
    end

    test "<Exec><![CDATA[asdfasdfasdf]]></Exec>" do
      data = T.create_cdata("asdfasdfasdf")
      exec = T.create_element("Exec", %{}, [data])
      expect = "<Exec><![CDATA[asdfasdfasdf]]></Exec>"
      assert T.to_string(exec) == expect
    end

    test "<Imporant><!-- why does xml have comments --></Imporant>" do
      comment = T.create_comment(" why does xml have comments ")
      important = T.create_element("Imporant", %{}, [comment])
      expect = "<Imporant><!-- why does xml have comments --></Imporant>"
      assert T.to_string(important) == expect
    end

    test "with xml declaration" do
      document = T.create_document(
        Data.declaration(version: "1.0", encoding: :utf8, standalone: true),
        nil,
        [Data.text(value: "body")]
      )
      expect = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>body"
      assert T.to_string(document) == expect
    end

    test "with doctype" do
      document = T.create_document(
        nil,
        Data.doctype(root_node: "html", dtds: [
          {:public, "a", "b"},
          {:system, "c"},
          {:inlined, "<!-- nothing -->"},
        ]),
        [T.create_element("html")]
      )
      expect = Enum.join([
        "<!DOCTYPE html PUBLIC \"a\" \"b\"",
        " SYSTEM \"c\"",
        " [<!-- nothing -->]>",
        "<html/>",
      ])
      assert T.to_string(document) == expect
    end

    test "<Hello:World>!</Hello:World>" do
      document =
        T.create_document(
          nil,
          nil,
          [T.create_ns_element(
            "Hello",
            "World",
            %{},
            [T.create_text("!")]
          )]
        )

      expect = "<Hello:World>!</Hello:World>"

      assert T.to_string(document) == expect
    end

    test "<t:foo t:bar=\"baz\"/>" do
      document =
        T.create_document(
          nil,
          nil,
          [T.create_ns_element(
            "t",
            "foo",
            %{ {"t", "bar"} => "baz" },
            []
          )]
        )

      expect = "<t:foo t:bar=\"baz\"/>"

      assert T.to_string(document) == expect
    end
  end

  describe "from_string" do
    test "<Hello>world</Hello>" do
      init = "<Hello>world</Hello>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          nil,
          [T.create_element(
            "Hello",
            %{},
            [T.create_text("world")]
          )]
        )

      assert T.equal(result, expect)
    end

    test "<?xml?>" do
      init = "<?xml?>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          Data.declaration(),
          nil,
          []
        )

      assert T.equal(result, expect)
    end

    test "<?xml version=\"1.0\" standalone=\"no\" encoding=\"UTF-8\"?>" do
      init = "<?xml version=\"1.0\" standalone=\"no\" encoding=\"UTF-8\"?>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          Data.declaration(
            version: "1.0",
            standalone: false,
            encoding: :utf8
          ),
          nil,
          []
        )

      assert T.equal(result, expect)
    end

    test "<!DOCTYPE document>" do
      init = "<!DOCTYPE document>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          Data.doctype(root_node: "document"),
          []
        )

      assert T.equal(result, expect)
    end

    test "<!DOCTYPE document PUBLIC \"some_path\" \"some_url\">" do
      init = "<!DOCTYPE document PUBLIC \"some_path\" \"some_url\">"
      {:ok, result} = T.from_string(init)

      # TODO
      expect =
        T.create_document(
          nil,
          Data.doctype(
            root_node: "document",
            dtds: [{:public, "some_path", "some_url"}]
          ),
          []
        )

      assert T.equal(result, expect)
    end

    test "<!DOCTYPE document SYSTEM \"somefile.dtd\">" do
      init = "<!DOCTYPE document SYSTEM \"somefile.dtd\">"
      {:ok, result} = T.from_string(init)

      # TODO
      expect =
        T.create_document(
          nil,
          Data.doctype(
            root_node: "document",
            dtds: [{:system, "somefile.dtd"}]
          ),
          []
        )

      assert T.equal(result, expect)
    end

    test "<!DOCTYPE document [a<!-- ]> -->b]>" do
      init = "<!DOCTYPE document [a<!-- ]> -->b]>"
      {:ok, result} = T.from_string(init)

      # TODO
      expect =
        T.create_document(
          nil,
          Data.doctype(
            root_node: "document",
            dtds: [{:inlined, "a<!-- ]> -->b"}]
          ),
          []
        )

      assert T.equal(result, expect)
    end

    # TODO parse comments before doctype & xml_declaration

    test "<Hello/>" do
      init = "<Hello/>"
      {:ok, result} = T.from_string(init)
      expect = T.create_document(nil, nil, [T.create_element("Hello")])
      assert T.equal(result, expect)
    end

    test "<Hello to=\"world\"/>" do
      init = "<Hello to=\"world\"/>"
      {:ok, result} = T.from_string(init)
      expect = T.create_document(
        nil,
        nil,
        [T.create_element("Hello", %{ "to" => "world" })]
      )
      assert T.equal(result, expect)
    end

    test "< Hello/>" do
      init = "< Hello/>"
      {:ok, result} = T.from_string(init)
      expect = T.create_document(nil, nil, [T.create_element("Hello")])
      assert T.equal(result, expect)
    end

    test "<Hello><World>!</World></Hello>" do
      init = "<Hello><World>!</World></Hello>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          nil,
          [T.create_element(
            "Hello",
            %{},
            [T.create_element("World", %{}, [T.create_text("!")])]
          )]
        )

      assert T.equal(result, expect)
    end

    test "<Hello:World>!</Hello:World>" do
      init = "<Hello:World>!</Hello:World>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          nil,
          [T.create_ns_element(
            "Hello",
            "World",
            %{},
            [T.create_text("!")]
          )]
        )

      assert T.equal(result, expect)
    end

    test "<boo t:bar=\"baz\"/>" do
      init = "<boo t:bar=\"baz\"/>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          nil,
          [T.create_element(
            "boo",
            %{ {"t", "bar"} => "baz" }
          )]
        )

      assert T.equal(result, expect)
    end

    test "<Group>do: <Hello/><World/></Group>" do
      init = "<Group>do: <Hello/><World/></Group>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          nil,
          [T.create_element("Group", %{}, [
            T.create_text("do: "),
            T.create_element("Hello"),
            T.create_element("World"),
          ])]
        )

      assert T.equal(result, expect)
    end

    test "<!-- hello world -->" do
      init = "<!-- hello world -->"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          nil,
          [T.create_comment(" hello world ")]
        )

      assert T.equal(result, expect)
    end

    test "<![CDATA[ayy lmao]]>" do
      init = "<![CDATA[ayy lmao]]>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          nil,
          [T.create_cdata("ayy lmao")]
        )

      assert T.equal(result, expect)
    end

    test "<![CDATA[]]]]><![CDATA[>]]>" do
      init = "<![CDATA[]]]]><![CDATA[>]]>"
      {:ok, result} = T.from_string(init)

      expect =
        T.create_document(
          nil,
          nil,
          [T.create_cdata("]]>")]
        )

      assert T.equal(result, expect)
    end

    test "document with declaration, doctype & body" do
      {:ok, result} = T.from_string(Enum.join([
        "<?xml version=\"1.0\"?>",
        "<!DOCTYPE document>",
        "<document>",
        "<hello \nto=\"world\"/>",
        "</document>",
      ]))

      expect =
        T.create_document(
          Data.declaration(version: "1.0"),
          Data.doctype(root_node: "document"),
          [T.create_element("document", %{}, [
            T.create_element("hello", %{ "to" => "world" }),
          ])]
        )

      assert T.equal(result, expect)
    end

    test "with comment before doctype" do
      {:ok, result} = T.from_string(Enum.join([
        "<?xml version=\"1.0\"?>",
        "<!-- hello -->",
        "<!DOCTYPE document>",
        "<document>",
        "<hello \nto=\"world\"/>",
        "</document>",
      ]))

      expect =
        T.create_document(
          Data.declaration(version: "1.0"),
          Data.doctype(root_node: "document"),
          [T.create_comment(" hello "),
           T.create_element("document", %{}, [
            T.create_element("hello", %{ "to" => "world" }),
          ])]
        )

      assert T.equal(result, expect)
    end
  end
end

defmodule Pandora.ParseTest do
  use ExUnit.Case

  require Pandora.Parse, as: P

  describe "parse_hint" do
    test "element" do
      parser = P.FromString.init("<Hello/>")
      {:ok, {_, h}} = P.parse_hint(parser)
      assert h == :element
    end

    test "comment" do
      parser = P.FromString.init("<!-- hello world -->")
      {:ok, {_, h}} = P.parse_hint(parser)
      assert h == :comment
    end

    test "cdata" do
      parser = P.FromString.init("<![CDATA[adsf]]>")
      {:ok, {_, h}} = P.parse_hint(parser)
      assert h == :cdata
    end

    test "text" do
      parser = P.FromString.init("asfa")
      {:ok, {_, h}} = P.parse_hint(parser)
      assert h == :text
    end
  end

  describe "skip_whitespace" do
    test "stops at non whitespace" do
      parser = P.FromString.init("hello")
      parser = P.skip_whitespace(parser)
      assert parser.cursor == 0
    end

    test "cursor progresses" do
      parser = P.FromString.init("  hello")
      parser = P.skip_whitespace(parser)
      assert parser.cursor == 2
    end

    test "cursor goes to end" do
      parser = P.FromString.init("       ")
      parser = P.skip_whitespace(parser)
      assert P.Parser.has_finished(parser)
    end
  end

  describe "skip_these_chunks" do
    test "['</', 'hello', '>']" do
      input = "</ hello >world"
      parser = P.FromString.init(input)
      chunks = ["</", "hello", ">"]
      {:ok, parser} = P.skip_these_chunks(parser, chunks)
      assert parser.cursor == 10
    end
  end

  describe "amount_til_end_of_whitespace" do
    test "without index" do
      input = "  <hello/>"
      parser = P.FromString.init(input)
      result = P.amount_til_end_of_whitespace(parser)
      assert result == {parser, 2}
    end

    test "with index" do
      input = "hello world"
      parser = P.FromString.init(input)
      result = P.amount_til_end_of_whitespace(parser, 5)
      assert result == {parser, 6}
    end
  end

  describe "are_these_the_next_chunks" do
    test "['<', '/', 'hello', '>']" do
      input = "<   /   hello >"
      parser = P.FromString.init(input)
      chunks = ["<", "/", "hello", ">"]
      {_, result, moved} = P.are_these_the_next_chunks(parser, chunks, 0)
      assert result
      assert moved == String.length(input)
    end

    test "['<', '/', 'hello', '>'] with offset" do
      input = "  <   /   hello >"
      parser = P.FromString.init(input)
      chunks = ["<", "/", "hello", ">"]
      {_, result, moved} = P.are_these_the_next_chunks(parser, chunks, 2)
      assert result
      assert moved == String.length(input)
    end
  end
end
