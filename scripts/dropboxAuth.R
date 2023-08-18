## Dropbox authentication

library(httr2)

client <- oauth_client(
    id = Sys.getenv('DROPBOX_KEY'),
    secret = Sys.getenv('DROPBOX_SECRET'),
    token_url = "https://api.dropboxapi.com/oauth2/token",
    name = 'Rstudio_TC'
)

token <- oauth_flow_auth_code(
    client, port = 43451,
    auth_url = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline"
)

saveRDS(token, 'token.RDS')
