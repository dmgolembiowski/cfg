require [ "regex", "variables", "fileinto", "mailbox" ];

# Split out mailing list posts to separate mailboxes:
if exists "list-id" {
    if header :regex "list-id" "<([a-z-]+)[.@]" {
        set :lower "listname" "${1}";
        fileinto :create "${listname}";
    }
}
