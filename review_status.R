#review status
rev<-catmaid_get_review_status(c(2109445, 2699293, 4291899))

rev$percentage <- (rev$reviewed/rev$total)*100

catmaid_get_review_status

function (skids, cut_node, pid = 1, conn = NULL, ...) 
{
  skids = catmaid_skids(skids, conn = conn, pid = pid)
  post_data = list()
  post_data[sprintf("skeleton_ids[%d]", seq_along(skids) - 
                      1)] = as.list(skids)
  path = sprintf("/%d/skeletons/review-status", pid)
  res = catmaid_fetch(path, body = post_data, include_headers = F, 
                      conn = conn, ...)
  res = list2df(res, cols = c("total", "reviewed"))
  missing_names = setdiff(as.character(skids), rownames(res))
  if (length(missing_names)) {
    warning("unable to identify ", length(missing_names), 
            " neuron(s).")
    res[missing_names, ] = NA
  }
  res[as.character(skids), ]
}