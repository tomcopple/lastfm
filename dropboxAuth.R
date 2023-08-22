## Dropbox auth
library(httr2)

dropbox_key = Sys.getenv('DROPBOX_KEY')
dropbox_secret = Sys.getenv('DROPBOX_SECRET')

dropboxClient <- oauth_client(
    id = dropbox_key,
    secret = dropbox_secret,
    token_url = "https://api.dropboxapi.com/oauth2/token",
    name = 'Rstudio_TC'
)

dropboxToken <- oauth_flow_auth_code(
    dropboxClient, port = 43451,
    auth_url = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline"
)

saveRDS(dropboxToken, 'dropbox.RDS')
