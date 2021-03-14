def log_in_address(session_obj, address)
  session_obj[:user_id] = address
end

def log_in(session_obj)
  log_in_admin(session_obj)
end

def log_in_admin(session_obj)
  log_in_address(session_obj, "admin@labdb")
end

def log_in_rw(session_obj)
  log_in_address(session_obj, "rw@labdb")
end

def log_in_ro(session_obj)
  log_in_address(session_obj, "ro@labdb")
end

def log_in_unauthorized(session_obj)
  log_in_address(session_obj, "unauthorized@labdb")
end
