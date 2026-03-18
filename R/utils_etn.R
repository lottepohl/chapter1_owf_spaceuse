# HELPER FUNCTIONS ----

#' Check input value against valid values
#'
#' @param x Value(s) to test.
#'   `NULL` values will automatically pass.
#' @param y Value(s) to test against.
#' @param name Name of the parameter.
#' @param lowercase If `TRUE`, the case of `x` and `y` values will ignored and
#'   `x` values will be returned lowercase.
#' @return Error or (lowercase) `x` values.
#' @family helper functions
#' @noRd
check_value <- function(x, y, name = "value", lowercase = FALSE) {
  # Remove NA from valid values
  y <- y[!is.na(y)]
  
  # Ignore case
  if (lowercase) {
    x <- tolower(x)
    y <- tolower(y)
  }
  
  # Check value(s) against valid values
  assertthat::assert_that(
    all(x %in% y), # Returns TRUE for x = NULL
    msg = glue::glue(
      "Can't find {name} `{x}` in: {y}",
      x = glue::glue_collapse(x, sep = "`, `", last = "` and/or `"),
      y = glue::glue_collapse(y, sep = ", ", width = 300)
    )
  )
  
  return(x)
}

#' Get credentials from environment variables, or set them manually
#'
#' By default, it's not necessary to set any values in this function as it's
#' used in the background by other functions. However, if you wish to provide
#' your username and password on a per function basis, this function allows you
#' to do so.
#'
#' @param username Username to the ETN database. By default read from the
#'   environment, but you can set it manually too.
#' @param password Password to the ETN database. By default read from the
#'   environment, but you can set it manually too.
#' @return A string as it is ingested by other functions that need
#'   authentication.
#' @family helper functions
#' @noRd
get_credentials <- function(username = Sys.getenv("ETN_USER"),
                            password = Sys.getenv("ETN_PWD")) {
  if (is.na(Sys.getenv("ETN_USER", unset = NA)) ||
      is.na(Sys.getenv("ETN_PWD", unset = NA))) {
    if (is_interactive()) {
      cli::cli_alert_info(
        "No credentials stored. See {.vignette etn::authentication} to configure
         credentials."
      )
      username <- prompt_user(prompt = "Please enter your username: ")
      password <- ask_pass()
    } else {
      # No credentials, not interactive
      cli::cli_abort(
        c(
          "No credentials stored. Can't prompt for credentials since this is not
           running in interactive mode.",
          "i" = "See {.vignette etn::authentication} to configure credentials."
        )
      )
    }
  }
  invisible(list(username = username, password = password))
}

#' Lifecycle warning for the deprecated connection argument
#'
#' @param function_identity Character of length one with the name
#'   of the function the warning is being generated from
#'
#' @family helper functions
#' @noRd
deprecate_warn_connection <- function() {
  lifecycle::deprecate_warn(
    when = "3.0.0",
    what = glue::glue("{function_identity}(connection)",
                      function_identity = get_parent_fn_name(depth = 2)
    ),
    details = cli::format_inline(
      "Database connections are handled automatically.
       See {.vignette etn::authentication} to configure credentials."
    ),
    env = rlang::caller_env(),
    user_env = rlang::caller_env(2),
    always = TRUE
  )
}

#' Get the name (symbol) of the parent function
#'
#' @return A length one Character with the name of the parent function.
#'
#' @family helper functions
#' @noRd
#'
#' @examples
#' child_fn <- function() {
#'   get_parent_fn_name()
#' }
#'
#' parent_fn <- function() {
#'   print(get_parent_fn_name())
#'   print(paste("nested:", child_fn()))
#' }
#'
#' parent_fn()
get_parent_fn_name <- function(depth = 1) {
  rlang::call_name(rlang::frame_call(frame = rlang::caller_env(n = depth)))
}

#' Determine testing status
#'
#' Copy of testthat::is_testing() implementation to avoid a runtime dependency
#' on testthat.
#'
#' @return `TRUE` inside a test.
#' @family helper functions
#' @noRd
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' Get the system's nodename
#'
#' A simple wrapper around `Sys.info()["nodename"]` to facilitate mocking in
#' tests.
#'
#' @returns Character of length one with the system's nodename.
#' @family helper functions
#' @noRd
#' @examples
#' get_nodename()
get_nodename <- function() {
  Sys.info()["nodename"]
}

#' Check if the local database protocol is available
#'
#' This function checks if the local database is available by checking the
#' nodename.
#'
#' The nodename check is a simple string check to see if the system's nodename
#' ends with "vliz.be", which is a convention for systems that have access to
#' the local database.#'
#'
#' @returns Logical. `TRUE` if the local database is available, `FALSE`
#'   otherwise.
#' @family helper functions
#' @noRd
#' @examples
#' # This should return FALSE unless you are running this example from the VLIZ
#' # RStudio server.
#' localdb_is_available()
localdb_is_available <- function() {
  # As discussed with VLIZ, all systems that have access to the local database
  # should have nodenames ending on vliz.be
  endsWith(get_nodename(), "vliz.be") |
    endsWith(get_nodename(), "europeantrackingnetwork.org")
}

#' Select the protocol to use
#'
#' The protocol is the way data is fetched. This is in addition to the source,
#' which is where the data comes from.
#'
#' This function is used to centrally control the decision tree for which
#' protocol to use for ´conduct_parent_to_helper()´. When there is a local
#' database connection available, use this by default. If not, use the OpenCPU
#' API. Both these protocols use the ETN database as a source.
#'
#' @returns Character of length one with one of the available protocols.
#'
#' @family helper functions
#' @noRd
select_protocol <- function() {
  # ALlow overwriting of protocol logic by environmental variable
  user_selected_protocol <- Sys.getenv("ETN_PROTOCOL",
                                       unset = "no_protocol_set")
  if (user_selected_protocol != "no_protocol_set") {
    return(user_selected_protocol)
  }
  
  # If there is a local database connection available, use it.
  if (localdb_is_available()) {
    return("localdb")
  }
  
  # Fallback on API
  return("opencpu")
}

#' Test if ETN credentials are stored
#'
#' This function checks if the ETN credentials are set in the environment
#' variables. It returns `TRUE` if both credentials are set, and `FALSE`
#' otherwise. This can be used in tests or examples to conditionally skip if the
#' credentials are not available.
#'
#' @returns A boolean indicating whether the ETN credentials are set in the
#'   environment variables.
#' @family helper functions
#' @noRd
credentials_are_set <- function(){
  nzchar(Sys.getenv("ETN_USER")) && nzchar(Sys.getenv("ETN_PWD"))
}

# WRAPPER FUNCTIONS ----

#' Wrapper of askpass::askpass
#'
#' This function is wrapped so it can be mocked in
#' `testhat::with_mocked_bindings()` and thus allows for testing the prompting
#' behaviour of `get_credentials()`
#'
#' @family wrappers
#' @noRd
ask_pass <- function(...) {
  askpass::askpass(...)
}

#' Wrapper of rlang::is_interactive
#'
#' This function is wrapped so it can be mocked in
#' `testhat::with_mocked_bindings()` and thus allows for testing the prompting
#' behaviour of `get_credentials()`
#'
#' @family wrappers
#' @noRd
is_interactive <- function(...) {
  rlang::is_interactive(...)
}

#' Wrapper for base::readline
#'
#' This function is wrapped because I find it easier to read, and so it can be
#' mocked in `testhat::with_mocked_bindings()` and thus allows for testing the prompting
#' behaviour of `get_credentials()`
#'
#' @family wrappers
#' @noRd
prompt_user <- function(...) {
  readline(...)
}

# rlang null handling -----------------------------------------------------
#' @importFrom rlang %||%
NULL


# onLoad ------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  # Memoisation: Every 15 minutes, check the deployed version of etnservice.
  # Checking this on every call would slow down the package.
  get_etnservice_version <<-
    memoise::memoise(get_etnservice_version,
                     cache = cachem::cache_mem(max_age = 60 * 15)
    )
  # Memoisation: only validate the login credentials every 15 minutes.
  validate_login <<-
    memoise::memoise(validate_login,
                     cache = cachem::cache_mem(max_age = 60 * 15)
    )
}

# API HELPER FUNCTIONS

#' Extract the OCPU temp key from a response object
#'
#' When posting a request to the opencpu api service without the json flag, a
#' response object is returned containing all the generated objects, with a
#' unique temp key in the path. To retrieve these objects in a subsequent GET
#' request, it is convenient to retrieve this temp key from the original
#' response object
#'
#' @param response The response resulting from a POST request to a opencpu api
#'   service
#'
#' @return the OCPU temp key to be used as part of a GET request to an opencpu
#'   api service
#' @family helper functions
#' @noRd
extract_temp_key <- function(response) {
  response |>
    httr2::resp_body_string() |>
    stringr::str_extract("(?<=tmp\\/).{15}(?=\\/)")
}

#' Retrieve the result of a function called to the opencpu api
#'
#' Fetch the result of an API call to OpenCPU
#'
#' This function is used internally to GET an evaluated object from an OpenCPU
#' api, to GET a result, you must of course POST a function call first
#'
#' @param temp_key the temp key returned from the POST request to the API
#' @param api_domain Character vector of the OpenCPU domain to use, defaults to
#'   "https://opencpu.lifewatch.be"
#' @param format Character vector of the format to use for the GET request,
#'   either "feather" or "rds", defaults to "feather". Note that feather is
#'   faster, but rds preserves more R specific object types.
#' @param return_url Logical. If `TRUE` fetching the result of the API call. The
#'   url of the result object is returned instead.
#' @param ... Query arguments to pass to OpenCPU. These are arguments to the
#'   writing functions from the [supported
#'   encoders](https://www.opencpu.org/api.html#api-formats). For example:
#'   `compression = "lz4"` in the case of `format = "feather"` is passed to
#'   `arrow::write_feather()`. Other encoders include `base::saveRDS`.
#'
#' @return the uncompressed object resulting form a GET request to the API. If
#'   `return_url` is `TRUE`, the url of the result object is returned instead.
#' @family helper functions
#' @noRd
#' @examples
#' \dontrun{
#' extract_temp_key(response) |> get_val()
#' }
#'
#' # using the opencpu test instance
#' api_url <- "https://cloud.opencpu.org/ocpu/library/stats/R/rnorm"
#' httr2::request(api_url) |>
#'  httr2::req_body_json(list(n = 10, mean = 5)) |>
#'  httr2::req_perform() |>
#'  extract_temp_key() |>
#'  get_val(api_domain = "https://cloud.opencpu.org/ocpu")
get_val <- function(temp_key,
                    api_domain = "https://opencpu.lifewatch.be",
                    format = c("feather", "rds"),
                    return_url = FALSE,
                    ...) {
  format <- rlang::arg_match(format)
  reading_function <-
    switch(format,
           "rds" = \(request) {
             raw_response <- request |>
               req_perform_opencpu() |>
               httr2::resp_body_raw()
             raw_connection <- rawConnection(raw_response)
             rds_response <-
               raw_connection |>
               gzcon() |>
               readRDS()
             # close connection
             close(raw_connection)
             # return R object
             rds_response
           },
           "feather" = \(request) {
             temp_featherfile <- withr::local_tempfile(fileext = ".feather")
             req_perform_opencpu(request, path = temp_featherfile)
             arrow::read_feather(temp_featherfile,
                                 mmap = FALSE)
           }
    )
  
  # early return in case of return_url
  if (return_url) {
    return(
      file.path(
        api_domain,
        "tmp", temp_key, "R", ".val", format
      )
    )
  }
  
  # request data and open connection
  query_request <-
    httr2::request(api_domain) |>
    httr2::req_url_path_append("tmp", temp_key, "R", ".val", format) |>
    httr2::req_url_query(...) |>
    httr2::req_retry(max_tries = 5)
  
  # read response via connection
  api_response <- query_request |>
    (\(x) reading_function(x))()
  
  # Return OpenCPU return object
  return(api_response)
}

#' Return the arguments as a named list of the parent environment
#'
#' Because the requests to the API are so similar, it's more DRY to pass the
#' function arguments of the parent function directly to the API, instead of
#' repeating them in the function body.
#'
#' @param depth Integer, the depth of the parent environement to extract. The
#' default of 1 means the direct parent environement, 2 means the parent of
#' the parent, etc.
#' @param compact Logical, if `TRUE` (default) `NULL` values are removed from the
#' returned list. Similar (but not identical to) to [purrr::compact()].
#'
#' @return a named list of name value pairs form the parent environement
#'
#' @family helper functions
#' @noRd
return_parent_arguments <- function(depth = 1, compact = TRUE) {
  # lock in the environment of the function we are being called in. Otherwise
  # lazy evaluation can cause trouble
  parent_env <- rlang::caller_env(n = depth)
  env_names <- rlang::env_names(parent_env)
  # set the environement names so lapply can output a names list
  names(env_names) <- env_names
  parent_arguments <- lapply(
    env_names,
    function(x) rlang::env_get(env = parent_env, nm = x)
  )
  if (compact) {
    purrr::compact(parent_arguments)
  }
  parent_arguments
}

#' Check if the provided credentials can be used to login via the API
#'
#' @param domain Character vector of the OpenCPU domain to use, defaults to
#'  "https://opencpu.lifewatch.be"
#' @param credentials A list containing the username and password to use for
#' login, defaults to `get_credentials()`
#'
#' @family helper functions
#' @noRd
validate_login <- function(domain = Sys.getenv("ETN_TEST_API",
                                               unset = "https://opencpu.lifewatch.be/library/etnservice/R"
),
credentials = get_credentials()) {
  # Placing a custom request because validate_login accepts username and
  # password directly in the body rather than as a credentials object like the
  # other functions.
  login_valid <- httr2::request(domain) |>
    httr2::req_url_path_append("validate_login", "json/") |>
    httr2::req_body_json(data = credentials) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  
  if (!login_valid) {
    rlang::abort(
      glue::glue(
        "Failed to log in with username: {get_credentials()$username}.",
        " Please check credentials."
      ),
      caller = rlang::env_parent()
    )
  }
  
  login_valid
}

#' Get the hostname from a URL string
#'
#' @param url_str A character string containing a URL
#'
#' @return The hostname extracted from the URL string including the scheme (eg.
#'   https)
#'
#' @family helper functions
#' @noRd
#' @examples
#' get_hostname("https://opencpu.lifewatch.be/library/etnservice/R")
get_hostname <- function(url_str) {
  # the hostname + everything in the path before `library`, because opencpu
  # doesn't need to be hosted directly on the hostname. Useful for testing on
  # other domains than etn.
  stringr::str_extract(url_str, ".+(?=library)")
}

#' Get the version of etnservice that was deployed on OpenCPU
#'
#' This function forwards a call to the API via `forward_to_api("get_version")`
#' to get the deployed version of etnservice on the OpenCPU deployment. It can
#' also request a list of function checksums.
#'
#' This function is useful because it allows us to mock the version of
#' etnservice for tests via `testhat::with_mocked_bindings()`. Thus allowing us
#' to test the error messaging in `conduct_parent_to_helpers()`.
#'
#' Setting `which = "local"` is the same as a direct call to
#' `etnservice::get_version()`. This option is still useful for mocking in
#' tests.
#'
#' @inheritDotParams forward_to_api format domain
#' @inheritDotParams get_val return_url
#' @inheritParams list_animal_ids
#' @param return_as Character, either "version" or "all", indicating if only the
#'   version number should be returned, or the full output of
#'   `etnservice::get_version()` (either locally or via the API).
#'
#' @returns Either a character string with the version number of etnservice. Or
#'   a list with the full output which includes the version number, And the
#'   checksums of all functions in etnservice.
#' @noRd
#' @family helper functions
get_etnservice_version <- function(return_as = c("version", "all"),
                                   ...) {
  return_as <- rlang::arg_match(return_as)
  # Get the full version information from the API
  pkg_version <-
    forward_to_api(
      "get_version",
      payload = list(),
      add_credentials = FALSE,
      format = "rds",
      ...
    )
  
  # Return either the version number or the full output
  switch(return_as,
         # coerce into character, packageVersion() returns other class.
         version = pkg_version$version,
         all = pkg_version
  )
}

#' Perform a request to OpenCPU to get a response
#'
#' This is a slight modification on httr2::req_perform() to allow for OpenCPU
#' error forwarding. OpenCPU doesn't stick to HTTP error codes, but assigns
#' different meanings and places R error messages in the body of HTTP 400
#' responses.
#'
#' @inheritParams httr2::req_perform
#' @param function_identity Character of length one with the name
#'  of the function the warning is being generated from, defaults to
#'  `get_parent_fn_name(depth = 3)`: the function calling this helper
#'  function.
#' @param ... Additional arguments passed on to `httr2::req_perform()`
#'
#' @return The response object from the request
#' @family helper functions
#' @noRd
req_perform_opencpu <- function(req,
                                function_identity = get_parent_fn_name(),
                                path = NULL,
                                ...) {
  resp <- tryCatch(
    httr2::req_perform(req, ...),
    httr2_http_400 = function(cnd) {
      rlang::abort(
        httr2::resp_body_string(cnd$resp),
        call = call(function_identity),
        footer = c(i = "This is an error forwarded via the API.")
      )
    },
    # OpenCPU reports server side errors as 502 and 503
    httr2_http_502 = function(cnd) {
      rlang::abort(
        c("Server side error",
          "*" = "Please try again.",
          "*" = "If the error persists, please report it to the package authors"
        )
      )
    },
    httr2_http_503 = function(cnd) {
      rlang::abort(
        c("Server side error",
          "*" = "Please try again.",
          "*" = "If the error persists, please report it to the package authors"
        )
      )
    }
  )
  if (!is.null(path)) {
    httr2::resp_body_raw(resp) |>
      writeBin(path)
  }
  resp
}

#' Forward function arguments to API and retrieve response
#'
#' @param function_identity Character vector of what function should be passed
#' @param payload Arguments to be passed to OpenCPU function
#' @param add_credentials Logical, if TRUE, then the credentials are added to
#'   the #'   payload. Defaults to TRUE. You want to turn is off for requests to
#'   libraries other than etnservice.
#' @param format Character, either "rds", "feather", or "json". This determines
#' how the result is fetched from the API. "feather" is faster, but does not
#' preserve all R object types. "rds" is slower, but preserves all R object
#' types. "json" fetches the result directly as JSON in a single request, but
#' only works for simple R object types that can be represented in JSON.
#'
#' @param domain Character vector of the OpenCPU domain to use, defaults to
#'   "https://opencpu.lifewatch.be/library/etnservice/R". A test domain can be
#'   set via the environmental variable `ETN_TEST_API`. VLIZ has requested the
#'   authors to not disclose this test url.
#' @param ... Additional arguments passed to the `get_val` function, such as
#'  `return_url`.
#' @return The same return object of the `function_identity` function
#'
#' @family helper functions
#' @noRd
forward_to_api <- function(
    function_identity,
    payload = list(),
    add_credentials = TRUE,
    format = c("rds", "feather", "json"),
    json = FALSE,
    domain = Sys.getenv("ETN_TEST_API",
                        unset = "https://opencpu.lifewatch.be/library/etnservice/R"
    ),
    ...) {
  format <- rlang::arg_match(format)
  # If the requested format is JSON, switch to a one step process.
  
  if (format == "json") {
    json <- TRUE
  }
  # Get credentials and attach to payload
  if (add_credentials) {
    # Get credentials out of .Renviron or prompt user.
    provided_credentials <- get_credentials()
    # Check if username/password are correct.
    # Skip check when testing, makes caching HTTP requests difficult due to
    # memoisation
    if (!is_testing()) {
      validate_login(credentials = provided_credentials)
    }
    # Add credentials to payload if correct and required.
    payload <-
      append(payload, list(credentials = provided_credentials), after = 0)
  }
  
  request <- httr2::request(domain) |>
    # Set endpoint based on the passed function_identity
    # NOTE trailing backslash is important for OpenCPU
    httr2::req_url_path_append(function_identity, "") |>
    httr2::req_body_json(payload) |>
    # Setup retry strategy
    httr2::req_retry(
      max_tries = 5,
      is_transient = function(resp) {
        # OpenCPU server side errors, sometimes transient
        httr2::resp_status(resp) %in% c(502, 503)
      }
    )
  
  # We can actually send a request and immediately fetch the result by requesting
  # JSON, but using a two step protocol allows us to get the exact R object that
  # was returned back via RDS (`get_val()`).
  if (json) {
    request <- request |>
      httr2::req_url_path_append("json/")
  }
  
  # Forward the function and arguments to the API: call 1, forward any R errors
  
  response <- req_perform_opencpu(request,
                                  function_identity = function_identity
  )
  
  
  if (json) {
    # return as a vector
    return(httr2::resp_body_json(response, simplifyVector = TRUE))
  } else {
    # Fetch the output from the API: call 2
    return(
      get_val(extract_temp_key(response),
              api_domain = get_hostname(domain),
              format = format,
              ...
      )
    )
  }
}