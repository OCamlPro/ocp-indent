let assigned_to u =
  Deferred.List.filter (Request_util.requests ()) ~f:(fun request ->
    if ...
    then ...
    else
      status_request ~request () ~msg_client:no_msg >>= fun status ->
      not (up_to_date_user status u))
