defmodule Pandora.Data do
  require Record

  @type attr_key :: String.t() | {String.t(), String.t()}
  @type attrs :: %{required(attr_key) => String.t()}

  @type dtd :: {:public, String.t(), String.t()} | {:system, String.t()} | {:inlined, String.t()}

  @type xml_encoding :: :utf8
  @type xml_node :: text | cdata | comment | element
  @type text :: {:text, String.t()}
  @type cdata :: {:cdata, String.t(), boolean}
  @type comment :: {:comment, String.t()}
  @type element :: {:element, String.t(), String.t(), attrs, :queue.queue()}

  @type xml_declaration :: {:declaration, String.t() | nil, xml_encoding | nil, boolean}
  @type doctype :: {:doctype, String.t(), [dtd]}

  @type document :: {:document, xml_declaration | nil, doctype | nil, :queue.queue()}

  Record.defrecord(:text, [:value])
  Record.defrecord(:cdata, [:value, :encoded])
  Record.defrecord(:comment, [:body])
  Record.defrecord(:element, [:name, :namespace, attributes: %{}, children: :queue.new()])
  Record.defrecord(:document, [:declaration, :doctype, :nodes])
  Record.defrecord(:doctype, [:root_node, dtds: []])
  Record.defrecord(:declaration, [:version, :encoding, :standalone])

  defguard is_element(n) when Record.is_record(n, :element)
  defguard is_document(n) when Record.is_record(n, :document)

  defguard is_node(n)
           when Record.is_record(n, :text) or Record.is_record(n, :element) or
                  Record.is_record(n, :cdata) or Record.is_record(n, :comment)
end
