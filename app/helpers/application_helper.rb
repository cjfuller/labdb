module ApplicationHelper
  def readonly?
    @readonly
  end

  def autotab
    @tab_index ||= 0
    @tab_index += 1
  end
end
