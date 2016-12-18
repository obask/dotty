package dotty.tools.dottydoc
package model
package comment

object BodyParsers {

  implicit class BodyToHtml(val body: Body) extends AnyVal {
    def toHtml(origin: Entity): String = {
      val inlineToHtml = InlineToHtml(origin)

      def bodyToHtml(body: Body): String =
        (body.blocks map blockToHtml).mkString

      def blockToHtml(block: Block): String = block match {
        case Title(in, 1)  => s"<h1>${inlineToHtml(in)}</h1>"
        case Title(in, 2)  => s"<h2>${inlineToHtml(in)}</h2>"
        case Title(in, 3)  => s"<h3>${inlineToHtml(in)}</h3>"
        case Title(in, _)  => s"<h4>${inlineToHtml(in)}</h4>"
        case Paragraph(in) => s"<p>${inlineToHtml(in)}</p>"
        case Code(data)    => s"""<pre><code class="scala">$data</code></pre>"""
        case UnorderedList(items) =>
          s"<ul>${listItemsToHtml(items)}</ul>"
        case OrderedList(items, listStyle) =>
          s"<ol class=${listStyle}>${listItemsToHtml(items)}</ol>"
        case DefinitionList(items) =>
          s"<dl>${items map { case (t, d) => s"<dt>${inlineToHtml(t)}</dt><dd>${blockToHtml(d)}</dd>" } }</dl>"
        case HorizontalRule() =>
          "<hr/>"
      }

      def listItemsToHtml(items: Seq[Block]) =
        items.foldLeft(""){ (list, item) =>
          item match {
            case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
              list + s"<li>${blockToHtml(item)}</li>"
            case Paragraph(inline) =>
              list + s"<li>${inlineToHtml(inline)}</li>"  // LIs are blocks, no need to use Ps
            case block =>
              list + s"<li>${blockToHtml(block)}</li>"
          }
      }

      bodyToHtml(body)
    }
  }

  case class InlineToHtml(origin: Entity) {
    def apply(inline: Inline) = toHtml(inline)

    def relativePath(target: Entity) =
      util.traversing.relativePath(origin, target)

    def toHtml(inline: Inline): String = inline match {
      case Chain(items)     => (items map toHtml).mkString
      case Italic(in)       => s"<i>${toHtml(in)}</i>"
      case Bold(in)         => s"<b>${toHtml(in)}</b>"
      case Underline(in)    => s"<u>${toHtml(in)}</u>"
      case Superscript(in)  => s"<sup>${toHtml(in)}</sup>"
      case Subscript(in)    => s"<sub>${toHtml(in) }</sub>"
      case Link(raw, title) => s"""<a href=$raw target="_blank">${toHtml(title)}</a>"""
      case Monospace(in)    => s"<code>${toHtml(in)}</code>"
      case Text(text)       => text
      case Summary(in)      => toHtml(in)
      case HtmlTag(tag)     => tag
      case EntityLink(target, link) => enityLinkToHtml(target, link)
    }

    def enityLinkToHtml(target: Inline, link: LinkTo) = link match {
      case Tooltip(_) => toHtml(target)
      case LinkToExternal(n, url) => s"""<a href="$url">$n</a>"""
      case LinkToEntity(t: Entity) => t match {
        // Entity is a package member
        case e: Entity with Members =>
          s"""<a href="${relativePath(t)}">${toHtml(target)}</a>"""
        // Entity is a Val / Def
        case x => x.parent.fold(toHtml(target)) { xpar =>
          s"""<a href="${relativePath(xpar)}#${x.name}">${toHtml(target)}</a>"""
        }
      }
    }
  }
}
