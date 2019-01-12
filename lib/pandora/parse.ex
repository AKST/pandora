defmodule Pandora.Parse do
  require Pandora.Data, as: Data
  require QueueWrapper, as: Queue

  @type result(t) :: {:ok, t} | {:error, any}
  @type parse_hint :: :text | :element | :cdata | :comment | :closing_node | :end_of_input

  defprotocol Parser do
    @spec move_cursor(parser :: any, amount :: pos_integer()) :: any
    def move_cursor(parser, amount)

    @spec slice_from_cursor_offset(
            parser :: any,
            offset :: pos_integer(),
            amount :: pos_integer()
          ) :: {any, String.t()}
    def slice_from_cursor_offset(parser, offset, amount)

    @spec lookup_from_cursor(parser :: any, index :: pos_integer()) :: {any, String.t()}
    def lookup_from_cursor(parser, index)

    @spec has_finished(parser :: any) :: {any, boolean}
    def has_finished(parser)
  end

  def slice_from_cursor(p, amount) do
    Parser.slice_from_cursor_offset(p, 0, amount)
  end

  defmodule FromString do
    defstruct [:input, :cursor]

    def init(input) do
      %__MODULE__{input: input, cursor: 0}
    end
  end

  defimpl Parser, for: FromString do
    def move_cursor(%FromString{} = p, amount) do
      %FromString{p | cursor: p.cursor + amount}
    end

    def slice_from_cursor_offset(%FromString{} = p, offset, amount) do
      {p, String.slice(p.input, offset + p.cursor, amount)}
    end

    def lookup_from_cursor(%FromString{} = p, index) do
      {p, String.at(p.input, p.cursor + index)}
    end

    def has_finished(%FromString{} = p) do
      {p, p.cursor >= String.length(p.input)}
    end

    def close(_), do: nil
  end

  @spec from_string(parser :: any) :: result(Data.document())
  def from_string(string) do
    parser = FromString.init(string)

    with {:ok, {_parser, document}} <- parse_document(parser) do
      {:ok, document}
    end
  end

  @spec parse_document(parser) :: result({parser, Data.document()}) when parser: any
  defp parse_document(parser) do
    with parser = skip_whitespace(parser),
         {:ok, {parser, declaration}} <- maybe_parse_declaration(parser),
         parser = skip_whitespace(parser),
         {:ok, {parser, comments}} <- parse_comments(parser),
         {:ok, {parser, doctype}} <- maybe_parse_doctype(parser),
         parser = skip_whitespace(parser),
         {:ok, {parser, nodes}} <- parse_nodes(parser) do
      nodes = Queue.join(comments, nodes)
      {:ok, {parser, Data.document(doctype: doctype, declaration: declaration, nodes: nodes)}}
    end
  end

  @spec maybe_parse_declaration(p) :: result({p, Data.doctype() | nil}) when p: any
  defp maybe_parse_declaration(p) do
    parse_xml_symbol = fn p ->
      case slice_from_cursor(p, 3) do
        {p, "xml"} ->
          p = Parser.move_cursor(p, 3)
          p = skip_whitespace(p)
          {:ok, p}

        {p, _} ->
          {:error, {:xml_declartion, p, :expected_xml_symbol_in_declaraction}}
      end
    end

    parse_declaraction_attributes = fn p ->
      supported_encodings = %{"UTF-8" => :utf8}
      standalone_swap = %{"YES" => true, "NO" => false}

      with {:ok, {p, attributes}} <- parse_attributes(p) do
        {v, attributes} = Map.pop(attributes, "version")
        {e, attributes} = Map.pop(attributes, "encoding")
        {s, attributes} = Map.pop(attributes, "standalone")
        e = if e != nil, do: String.upcase(e), else: nil
        s = if s != nil, do: String.upcase(s), else: nil

        if e == nil || Map.has_key?(supported_encodings, e) do
          if s == nil || Map.has_key?(standalone_swap, s) do
            e = Map.get(supported_encodings, e)
            s = Map.get(standalone_swap, s)

            # fails if the any attributes remain, since they're not
            # part of the XML standard.
            if attributes == %{},
              do: {:ok, {p, v, e, s}},
              else: {:error, {:xml_declartion, p, {:unexpected_attributes, attributes}}}
          else
            {:error, {:xml_declartion, p, {:invalid, e}}}
          end
        else
          {:error, {:xml_declartion, p, {:unsupported_encoding, e}}}
        end
      end
    end

    parse_closing_symbol = fn p ->
      case slice_from_cursor(p, 2) do
        {p, "?>"} ->
          p = skip_whitespace(p)
          p = Parser.move_cursor(p, 2)
          {:ok, p}

        {p, _} ->
          {:error, {:xml_declartion, p, :expected_xml_declaration_closing_tag}}
      end
    end

    case slice_from_cursor(p, 2) do
      {p, "<?"} ->
        p = Parser.move_cursor(p, 2)
        p = skip_whitespace(p)

        with {:ok, p} <- parse_xml_symbol.(p),
             {:ok, {p, v, e, s}} <- parse_declaraction_attributes.(p),
             {:ok, p} <- parse_closing_symbol.(p) do
          {:ok, {p, Data.declaration(version: v, standalone: s, encoding: e)}}
        end

      {p, _} ->
        {:ok, {p, nil}}
    end
  end

  @spec maybe_parse_doctype(p) :: result({p, Data.doctype() | nil}) when p: any
  defp maybe_parse_doctype(p) do
    parse_doctype_symbol = fn p ->
      {p, slice} = slice_from_cursor(p, 7)

      if String.upcase(slice) == "DOCTYPE" do
        p = Parser.move_cursor(p, 7)
        {:ok, p}
      else
        {:error, {:doctype_error, p, :expected_doctype_symbol}}
      end
    end

    parse_root_node_name = fn p ->
      {p, name} = parse_identifer_token(p)

      if name != "" do
        {:ok, {p, name}}
      else
        {:error, {:doctype_error, p, :expected_root_node_name}}
      end
    end

    parse_inlined_dtd_impl = fn f, p, chunks, offset ->
      case Parser.slice_from_cursor_offset(p, offset, 1) do
        {p, "]"} ->
          {p, last_chunk} = slice_from_cursor(p, offset)
          p = Parser.move_cursor(p, offset + 1)
          {:ok, {p, join_in_reverse([last_chunk | chunks])}}

        {p, _} ->
          case Parser.slice_from_cursor_offset(p, offset, 4) do
            {p, "<!--"} ->
              {p, text_chunk} = slice_from_cursor(p, offset)
              p = Parser.move_cursor(p, offset)

              with {:ok, {p, {:comment, comment_body}}} <- parse_comment(p) do
                comment_chunk = "<!--#{comment_body}-->"
                chunks = [comment_chunk, text_chunk | chunks]
                f.(f, p, chunks, 0)
              end

            {p, _} ->
              f.(f, p, chunks, offset + 1)
          end
      end
    end

    parse_inlined_dtd = fn p ->
      p = Parser.move_cursor(p, 1)
      parse_inlined_dtd_impl.(parse_inlined_dtd_impl, p, [], 0)
    end

    maybe_parse_dtds = fn f, p, dtds ->
      case slice_from_cursor(p, 1) do
        {p, "["} ->
          with {:ok, {p, inlined}} <- parse_inlined_dtd.(p) do
            f.(f, p, [{:inlined, inlined} | dtds])
          end

        {p, _} ->
          {p, modifier} = parse_identifer_token(p)

          case String.upcase(modifier) do
            "" ->
              {:ok, {p, Enum.reverse(dtds)}}

            "SYSTEM" ->
              p = skip_whitespace(p)

              with {:ok, {p, dtd_url}} <- parse_quoted_string(p) do
                p = skip_whitespace(p)
                f.(f, p, [{:system, dtd_url} | dtds])
              end

            "PUBLIC" ->
              p = skip_whitespace(p)

              with {:ok, {p, dtd_location}} <- parse_quoted_string(p),
                   p = skip_whitespace(p),
                   {:ok, {p, dtd_url}} <- parse_quoted_string(p) do
                p = skip_whitespace(p)
                f.(f, p, [{:public, dtd_location, dtd_url} | dtds])
              end

            other ->
              {:error, {:doctype_error, p, {:unexpected_dtd_start_token, other, dtds}}}
          end
      end
    end

    parse_doctype_closing_token = fn p ->
      {p, token} = slice_from_cursor(p, 1)

      if token == ">" do
        p = Parser.move_cursor(p, 1)
        {:ok, p}
      else
        {:error, {:doctype_error, p, :expected_end_of_doctype}}
      end
    end

    case slice_from_cursor(p, 2) do
      {p, "<!"} ->
        case Parser.slice_from_cursor_offset(p, 2, 2) do
          {p, "--"} ->
            {:ok, {p, nil}}

          {p, "[C"} ->
            {:ok, {p, nil}}

          {p, _} ->
            p = Parser.move_cursor(p, 2)
            p = skip_whitespace(p)

            with {:ok, p} <- parse_doctype_symbol.(p),
                 p = skip_whitespace(p),
                 {:ok, {p, root_node}} <- parse_root_node_name.(p),
                 p = skip_whitespace(p),
                 {:ok, {p, dtds}} <- maybe_parse_dtds.(maybe_parse_dtds, p, []),
                 {:ok, p} <- parse_doctype_closing_token.(p) do
              {:ok, {p, Data.doctype(root_node: root_node, dtds: dtds)}}
            end
        end

      {p, _} ->
        {:ok, {p, nil}}
    end
  end

  @spec parse_nodes(p) :: result({p, Queue.t()}) when p: any
  defp parse_nodes(p) do
    recursive = fn f, p, nodes ->
      with_parsing_fn = fn p, parse_fn ->
        with {:ok, {p, node}} <- parse_fn.(p) do
          nodes = Queue.in_rear(node, nodes)
          f.(f, p, nodes)
        end
      end

      case parse_hint(p) do
        {:ok, {p, :element}} ->
          with_parsing_fn.(p, &parse_element/1)

        {:ok, {p, :text}} ->
          with_parsing_fn.(p, &parse_text/1)

        {:ok, {p, :comment}} ->
          with_parsing_fn.(p, &parse_comment/1)

        {:ok, {p, :cdata}} ->
          with_parsing_fn.(p, &parse_cdata/1)

        {:ok, {p, :closing_node}} ->
          {:ok, {p, nodes}}

        {:ok, {p, :end_of_input}} ->
          {:ok, {p, nodes}}
      end
    end

    recursive.(recursive, p, Queue.new())
  end

  @spec parse_element(p) :: result({p, Data.xml_node()}) when p: any
  defp parse_element(p) do
    consume_alpha_token = fn p ->
      consume_while(p, fn c ->
        c != " " && c != ":" && c != nil && c != "/" && c != ">"
      end)
    end

    parse_tag_name = fn p ->
      {p, token_one} = consume_alpha_token.(p)
      p = skip_whitespace(p)

      case slice_from_cursor(p, 1) do
        {p, ":"} ->
          # skip the ':'
          p = Parser.move_cursor(p, 1)
          p = skip_whitespace(p)
          {p, token_two} = consume_alpha_token.(p)
          {p, token_two, token_one}

        {p, _} ->
          {p, token_one, nil}
      end
    end

    # skip the '<'
    p = Parser.move_cursor(p, 1)
    p = skip_whitespace(p)
    {p, tag_name, namespace} = parse_tag_name.(p)
    p = skip_whitespace(p)

    with {:ok, {p, attributes}} <- parse_attributes(p) do
      p = skip_whitespace(p)

      case Parser.lookup_from_cursor(p, 0) do
        {p, "/"} ->
          p = Parser.move_cursor(p, 1)
          p = skip_whitespace(p)

          with {:ok, p} <- skip_these_chunks(p, [">"]) do
            {:ok,
             {p,
              Data.element(
                name: tag_name,
                namespace: namespace,
                attributes: attributes
              )}}
          end

        {p, ">"} ->
          p = Parser.move_cursor(p, 1)
          p = skip_whitespace(p)

          chunks =
            if namespace == nil,
              do: ["</", tag_name, ">"],
              else: ["</", namespace, ":", tag_name, ">"]

          with {:ok, {p, nodes}} <- parse_nodes(p),
               {:ok, p} <- skip_these_chunks(p, chunks) do
            {:ok,
             {p,
              Data.element(
                name: tag_name,
                namespace: namespace,
                attributes: attributes,
                children: nodes
              )}}
          end

        {_p, _c} ->
          {:error, :expected_end_of_node}
      end
    end
  end

  @spec parse_text(p) :: result({p, Data.xml_node()}) when p: any
  defp parse_text(p) do
    recursive = fn f, p, index ->
      case parse_hint(p, index) do
        {:ok, {p, :text}} ->
          f.(f, p, index + 1)

        {:ok, _} ->
          {p, text} = slice_from_cursor(p, index)
          p = Parser.move_cursor(p, index)
          {:ok, {p, Data.text(value: text)}}
      end
    end

    recursive.(recursive, p, 1)
  end

  @spec parse_comment(p) :: result({p, Data.xml_node()}) when p: any
  defp parse_comment(p) do
    p = Parser.move_cursor(p, 4)

    recursive = fn f, p, index ->
      case Parser.slice_from_cursor_offset(p, index, 3) do
        {p, "-->"} ->
          {p, body} = slice_from_cursor(p, index)
          p = Parser.move_cursor(p, index + 3)
          {:ok, {p, Data.comment(body: body)}}

        {p, chunk} ->
          if String.length(chunk) == 3 do
            f.(f, p, index + 1)
          else
            {:error, :unexpected_end_of_comment}
          end
      end
    end

    recursive.(recursive, p, 0)
  end

  @spec parse_comments(p) :: result({p, Queue.t()}) when p: any
  defp parse_comments(p) do
    recursive = fn f, p, q ->
      case parse_hint(p) do
        {:ok, {p, :comment}} ->
          with {:ok, {p, comment}} <- parse_comment(p) do
            q = Queue.in_rear(comment, q)
            p = skip_whitespace(p)
            f.(f, p, q)
          end

        {:ok, {p, _}} ->
          {:ok, {p, q}}
      end
    end

    recursive.(recursive, p, Queue.new())
  end

  @spec parse_cdata(p) :: result({p, Data.xml_node()}) when p: any
  defp parse_cdata(p) do
    # skip the <![CDATA[
    p = Parser.move_cursor(p, 9)

    recursive = fn f, p, read_chunks, read_from, read_offset ->
      read_start = read_from + read_offset

      case Parser.slice_from_cursor_offset(p, read_start, 3) do
        {p, "]]>"} ->
          {p, chunk} = Parser.slice_from_cursor_offset(p, read_from, read_offset)
          read_chunks = [chunk | read_chunks]

          case Parser.slice_from_cursor_offset(p, read_start + 3, 9) do
            {p, "<![CDATA["} ->
              read_from = read_start + 3 + 9
              f.(f, p, read_chunks, read_from, 0)

            {p, _} ->
              # move the cursor to the end of the CDATA
              p = Parser.move_cursor(p, read_start + 3)
              combined_chunks = join_in_reverse(read_chunks)
              {:ok, {p, Data.cdata(value: combined_chunks, encoded: true)}}
          end

        {p, _} ->
          f.(f, p, read_chunks, read_from, read_offset + 1)
      end
    end

    recursive.(recursive, p, [], 0, 0)
  end

  @spec parse_attributes(p) :: result({p, Data.attrs()}) when p: any
  defp parse_attributes(p) do
    parse_attributes(p, %{})
  end

  @spec parse_attributes(p, attributes :: Data.attrs()) :: result({p, Data.attrs()}) when p: any
  defp parse_attributes(p, attributes) do
    consume_alpha_token = fn p ->
      consume_while(p, &(&1 != nil && is_alphanum(&1)))
    end

    parse_attribute_name = fn p ->
      {p, token_one} = consume_alpha_token.(p)
      p = skip_whitespace(p)

      case slice_from_cursor(p, 1) do
        {p, ":"} ->
          # skip the ':'
          p = Parser.move_cursor(p, 1)
          p = skip_whitespace(p)
          {p, token_two} = consume_alpha_token.(p)
          {p, {token_one, token_two}}

        {p, _} ->
          {p, token_one}
      end
    end

    {p, next_char} = Parser.lookup_from_cursor(p, 0)

    # may want to skip whitespace if
    # this isn't the first attribute.
    p =
      if attributes != %{},
        do: skip_whitespace(p),
        else: p

    if is_alpha(next_char) do
      {p, key} = parse_attribute_name.(p)

      if !Map.has_key?(attributes, key) do
        p = skip_whitespace(p)

        with {:ok, p} <- skip_these_chunks(p, ["="]),
             {:ok, {p, value}} <- parse_quoted_string(p) do
          parse_attributes(p, Map.put(attributes, key, value))
        end
      else
        {:error, {:attributes, :duplicate_attribute, key}}
      end
    else
      {:ok, {p, attributes}}
    end
  end

  @spec parse_hint(p) :: result({p, parse_hint}) when p: any
  def parse_hint(p) do
    parse_hint(p, 0)
  end

  @spec parse_hint(p, start_from :: non_neg_integer) :: result({p, parse_hint}) when p: any
  def parse_hint(p, start_from) do
    case Parser.lookup_from_cursor(p, 0 + start_from) do
      {p, nil} ->
        {:ok, {p, :end_of_input}}

      {p, "<"} ->
        case Parser.lookup_from_cursor(p, 1 + start_from) do
          {p, "!"} ->
            case Parser.lookup_from_cursor(p, 2 + start_from) do
              {p, "-"} ->
                case Parser.lookup_from_cursor(p, 2 + start_from) do
                  {p, "-"} -> {:ok, {p, :comment}}
                  {p, _} -> {:ok, {p, :text}}
                end

              {p, "["} ->
                case are_these_the_next_chunks(p, ["CDATA["], 3 + start_from) do
                  {p, true, _} -> {:ok, {p, :cdata}}
                  {p, false, _} -> {:ok, {p, :text}}
                end

              {p, _} ->
                {:ok, {p, :text}}
            end

          {p, _} ->
            # Skip the legal whitespace before the character
            # without consuming it in the hint making function
            {p, bump} = amount_til_end_of_whitespace(p, 1 + start_from)

            case Parser.lookup_from_cursor(p, start_from + bump) do
              {p, "/"} ->
                {:ok, {p, :closing_node}}

              {p, "_"} ->
                {:ok, {p, :element}}

              {p, char} ->
                case is_alpha(char) do
                  true -> {:ok, {p, :element}}
                  false -> {:ok, {p, :text}}
                end
            end
        end

      {p, _} ->
        {:ok, {p, :text}}
    end
  end

  @spec parse_identifer_token(p) :: {p, String.t()} when p: any
  def parse_identifer_token(p) do
    consume_while(p, fn c ->
      c != " " && c != nil && c != "/" && c != ">"
    end)
  end

  @spec parse_quoted_string(p) :: result({p, String.t()}) when p: any
  def parse_quoted_string(p) do
    with {:ok, p} <- skip_these_chunks(p, ["\""]),
         {p, value} = consume_while(p, &(&1 != nil && &1 != "\"")),
         {:ok, p} <- skip_these_chunks(p, ["\""]) do
      {:ok, {p, value}}
    end
  end

  @spec skip_these_chunks(p, chunks :: [String.t()]) :: result(p) when p: any
  def skip_these_chunks(p, chunks) do
    {p, result, move_cursor_by} = are_these_the_next_chunks(p, chunks, 0)

    if result do
      p = Parser.move_cursor(p, move_cursor_by)
      {:ok, p}
    else
      {:error, {:unexpected_chunks, p, chunks}}
    end
  end

  @spec are_these_the_next_chunks(
          p,
          chunks :: nonempty_list(String.t()),
          offset :: non_neg_integer
        ) :: {p, boolean, non_neg_integer}
        when p: any
  def are_these_the_next_chunks(p, [], offset) do
    {p, true, offset}
  end

  def are_these_the_next_chunks(p, [head_chunk | remaining_chunks], offset) do
    chunk_length = String.length(head_chunk)
    {p, read_chunk} = Parser.slice_from_cursor_offset(p, offset, chunk_length)

    if read_chunk == head_chunk do
      {p, offset} = amount_til_end_of_whitespace(p, offset + chunk_length)
      are_these_the_next_chunks(p, remaining_chunks, offset)
    else
      {p, false, offset}
    end
  end

  @spec amount_til_end_of_whitespace(parser :: any, index :: non_neg_integer) ::
          {any, non_neg_integer}
  def amount_til_end_of_whitespace(p, index) do
    {p, character} = Parser.lookup_from_cursor(p, index)

    case character do
      " " -> amount_til_end_of_whitespace(p, index + 1)
      "\t" -> amount_til_end_of_whitespace(p, index + 1)
      "\n" -> amount_til_end_of_whitespace(p, index + 1)
      "\r" -> amount_til_end_of_whitespace(p, index + 1)
      "\f" -> amount_til_end_of_whitespace(p, index + 1)
      nil -> {p, index}
      _ -> {p, index}
    end
  end

  @spec amount_til_end_of_whitespace(parser :: any) :: {any, non_neg_integer}
  def amount_til_end_of_whitespace(p) do
    amount_til_end_of_whitespace(p, 0)
  end

  @spec skip_whitespace(p) :: p when p: any
  def skip_whitespace(p) do
    {p, n} = amount_til_end_of_whitespace(p)
    Parser.move_cursor(p, n)
  end

  @spec consume_while(p, predicate :: (String.t() -> boolean)) :: {p, String.t()} when p: any
  defp consume_while(p, predicate) do
    consume_while(p, predicate, 0)
  end

  @spec consume_while(p, predicate :: (String.t() -> boolean), index :: non_neg_integer) ::
          {p, String.t()}
        when p: any
  defp consume_while(p, predicate, index) do
    {p, character} = Parser.lookup_from_cursor(p, index)

    cond do
      predicate.(character) ->
        consume_while(p, predicate, index + 1)

      index == 0 ->
        {p, ""}

      true ->
        {p, string} = slice_from_cursor(p, index)
        p = Parser.move_cursor(p, index)
        {p, string}
    end
  end

  @spec is_alpha(character_s :: String.t()) :: boolean
  defp is_alpha(""), do: false

  defp is_alpha(character_s) do
    [character | _] = to_charlist(character_s)
    [lil_a, lil_z, big_a, big_z] = 'azAZ'

    cond do
      character >= lil_a && character <= lil_z -> true
      character >= big_a && character <= big_z -> true
      true -> false
    end
  end

  @spec join_in_reverse(a :: list(String.t())) :: String.t()
  defp join_in_reverse(chunks) do
    Enum.reduce(chunks, "", &"#{&1}#{&2}")
  end

  @spec is_alphanum(character_s :: String.t()) :: boolean
  defp is_alphanum(""), do: false

  defp is_alphanum(character_s) do
    [character | _] = to_charlist(character_s)
    [lil_a, lil_z, big_a, big_z, zero, nine] = 'azAZ09'

    cond do
      character >= zero && character <= nine -> true
      character >= lil_a && character <= lil_z -> true
      character >= big_a && character <= big_z -> true
      true -> false
    end
  end
end
