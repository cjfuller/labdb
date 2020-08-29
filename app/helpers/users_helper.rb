module UsersHelper
  def badge_class_for_boolean(bool)
    if bool
      "badge badge-success noclick"
    else
      "badge badge-important noclick"
    end
  end
end
