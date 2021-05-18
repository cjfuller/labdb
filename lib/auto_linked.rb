require "action_dispatch/routing/url_for"

require "object_naming"

class LinkableString
  include Rails.application.routes.url_helpers

  def controller
    nil
  end

  def initialize(str)
    @str = str
  end

  def scan_matchobjs(regex)
    matches = []
    last_pos = 0
    while m = regex.match(@str, last_pos)
      last_pos = m.end(0)
      yield m
      matches << m
    end
    matches
  end

  def lazy_item_links()
    matches = []
    seen = Set.new
    Naming::NAMES_LOOKUP.each_key do |k|
      scan_matchobjs(/#{k}N?\W*(\d+)/) do |m|
        if !seen.include?(m[0])
          seen << m[0]
          matches << [
            m[0], url_for(
              controller: "application",
              action: :show_by_name,
              name: m[0],
              only_path: true,
            )
          ]
        end
      end
    end
    matches
  end

  def item_links(items: false)
    matches = []
    Naming::NAMES_LOOKUP.each_key do |k|
      scan_matchobjs(/#{k}N?\W*(\d+)/) do |m|
        cls = Naming.named_class_for(k).constantize
        id = cls.where(cls.number_field_name => m[1]).first
        if id
          if items
            matches << id
          else
            matches << [
              m[0], url_for(
                controller: Naming.named_class_for(k).downcase.pluralize,
                action: :show,
                id: id,
                only_path: true,
              ),
            ]
          end
        end
      end
    end
    matches
  end

  def sub_labdb_links
    self.lazy_item_links.each do |lnk|
      @str = @str.gsub(/#{lnk[0]}(?!\d)/, "<a class=\"auto-link\" href=\"#{lnk[1]}\">#{lnk[0]}</a>")
    end
    self
  end

  def to_s
    @str
  end
end

class String
  def labdb_auto_link
    ls = LinkableString.new(self)
    ls.sub_labdb_links
    ls.to_s
  end

  def item_links(items: false)
    ls = LinkableString.new(self)
    ls.item_links(items: items)
  end

  def lazy_item_links()
    ls = LinkableString.new(self)
    ls.lazy_item_links()
  end
end
