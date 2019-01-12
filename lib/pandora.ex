defmodule Pandora do
  require QueueWrapper, as: Queue
  require Pandora.Data, as: Data
  require Pandora.Parse, as: Parse

  def create_document(declaration, doctype, nodes) do
    Data.document(
      declaration: declaration,
      doctype: doctype,
      nodes: Queue.from_list(nodes)
    )
  end

  def create_element(tag_name, attributes \\ %{}, children \\ [])
      when is_binary(tag_name) and is_map(attributes) and is_list(children) do
    Data.element(
      name: tag_name,
      namespace: nil,
      attributes: attributes,
      children: Queue.from_list(children)
    )
  end

  def create_ns_element(ns, tn, attributes \\ %{}, children \\ [])
      when is_binary(tn) and is_binary(ns) and is_map(attributes) and is_list(children)  do
    Data.element(
      name: tn,
      namespace: ns,
      attributes: attributes,
      children: Queue.from_list(children)
    )
  end

  def create_text(body) when is_binary(body) do
    Data.text(value: body)
  end

  def create_comment(body) when is_binary(body) do
    Data.comment(body: body)
  end

  def create_cdata(body, encoded \\ true) when is_binary(body) do
    Data.cdata(value: body, encoded: encoded)
  end

  def prepend(root, path, new_node) when not Data.is_node(new_node) do
    {:error, {:xml, {:bad_prepend, root, path, new_node}}}
  end

  def prepend(root, path, new_node) do
    prepend_impl(root, path, new_node, [])
  end

  defp prepend_impl(root, [], new_node, _prev) when Data.is_element(root) do
    children = Queue.in_front(new_node, Data.element(root, :children))
    {:ok, Data.element(root, children: children)}
  end

  defp prepend_impl(root, [x | xs], new_node, prev) when Data.is_element(root) do
    with {:ok, {index, child}} <- get_child_with_replace(root, x),
         {:ok, child} <- prepend_impl(child, xs, new_node, [x | prev]) do
      children = Data.element(root, :children)
      children = Queue.replace_at(children, index, child)
      {:ok, Data.element(root, children: children)}
    end
  end

  defp get_child_with_replace(root, match) do
    init = {:not_found, 0}
    children = Data.element(root, :children)

    result =
      Queue.reduce_while(children, init, fn
        element, {:not_found, index} ->
          if match_element(element, match),
            do: {:halt, {:found, index, element}},
            else: {:cont, {:not_found, index + 1}}
      end)

    case result do
      {:not_found, _} ->
        {:error, {:get_child_with_replace, {:failed_to_find, root, :tag_name}}}

      {:found, index, element} ->
        {:ok, {index, element}}
    end
  end

  defp match_element(node, {:el, tag_name})
       when Data.is_element(node)
       when tag_name == Data.element(node, :name) do
    true
  end

  defp match_element(_, _) do
    false
  end

  @spec from_string(string :: String.t()) :: {:ok, Data.document} | {:error, any}
  def from_string(string) do
    Parse.from_string(string)
  end

  def to_string({:document, declaration, doctype, nodes}) do
    with_state = fn state, next -> "#{state}#{next}" end
    state = ""
    state = to_string_xml_declaration(declaration, state, with_state)
    state = to_string_doctype(doctype, state, with_state)
    Queue.reduce(nodes, state, fn node, state ->
      to_string_impl(node, state, with_state)
    end)
  end

  def to_string(node) do
    with_state = fn state, next -> "#{state}#{next}" end
    to_string_impl(node, "", with_state)
  end

  defp to_string_xml_declaration(nil, state, _), do: state
  defp to_string_xml_declaration(declaration, state, with_state) do
    {:declaration, version, encoding, standalone} = declaration
    state = with_state.(state, "<?xml")
    state = if version != nil,
      do: with_state.(state, " version=\"#{version}\""),
      else: state
    state = case encoding do
      nil -> state
      :utf8 -> with_state.(state, " encoding=\"UTF-8\"")
    end
    state = case standalone do
      nil -> state
      true -> with_state.(state, " standalone=\"yes\"")
      false -> with_state.(state, " standalone=\"no\"")
    end
    with_state.(state, "?>")
  end

  defp to_string_doctype(nil, state, _), do: state
  defp to_string_doctype(doctype, state, with_state) do
    {:doctype, root_node, dtds} = doctype
    state = with_state.(state, "<!DOCTYPE #{root_node}")
    state = Enum.reduce(dtds, state, fn dtd, state ->
      case dtd do
        {:public, location, url} ->
          with_state.(state, " PUBLIC \"#{location}\" \"#{url}\"")

        {:system, url} ->
          with_state.(state, " SYSTEM \"#{url}\"")

        {:inlined, content} ->
          with_state.(state, " [#{content}]")
      end
    end)
    with_state.(state, ">")
  end

  defp to_string_impl(node, state, with_state) do
    tag_name_to_string = fn
      (nil, name) -> name
      (namespace, name) -> "#{namespace}:#{name}"
    end

    attributes_to_string = fn (state, attributes) ->
      Enum.reduce(attributes, state, fn
        ({{nk, ak}, v}, state) -> with_state.(state, " #{nk}:#{ak}=\"#{v}\"")
        ({k, v}, state) -> with_state.(state, " #{k}=\"#{v}\"")
      end)
    end

    case node do
      {:text, value} ->
        with_state.(state, value)

      {:cdata, value, true} ->
        with_state.(state, "<![CDATA[#{value}]]>")

      {:comment, body} ->
        with_state.(state, "<!--#{body}-->")

      {:element, name, ns, attributes, children} ->
        tag_name_str = tag_name_to_string.(ns, name)
        if Queue.length(children) > 0 do
          state = with_state.(state, "<#{tag_name_str}")
          state = attributes_to_string.(state, attributes)
          state = with_state.(state, ">")

          state =
            Queue.reduce(children, state, fn child, state ->
              to_string_impl(child, state, with_state)
            end)

          with_state.(state, "</#{tag_name_str}>")
        else
          state = with_state.(state, "<#{tag_name_str}")
          state = attributes_to_string.(state, attributes)
          with_state.(state, "/>")
        end
    end
  end

  def equal(left, right) do
    case {left, right} do
      {{:text, a}, {:text, b}} ->
        a == b

      {{:comment, a}, {:comment, b}} ->
        a == b

      {{:cdata, aa, ab}, {:cdata, ba, bb}} ->
        aa == ba && ab == bb

      {{:element, a_n, a_ns, a_a, a_c}, {:element, b_n, b_ns, b_a, b_c}} ->
        a_n == b_n && a_ns == b_ns && a_a == b_a && Queue.equal(a_c, b_c, &equal/2)

      {{:document, aa, ab, ac}, {:document, ba, bb, bc}} ->
        aa == ba && ab == bb && Queue.equal(ac, bc, &equal/2)

      _ ->
        false
    end
  end
end
