# README

# Description

Simple Erlang Chat

Swagger UI - [http://localhost:8998/swagger](http://localhost:8998/swagger)

erl -name node1@127.0.0.1 -setcookie eschat
erl -name node2@127.0.0.1 -setcookie eschat

## BD structure

- **User** – id bigserial, login varchar, pass varchar
- **Session** – id varchar, user_id, bigint, active_to timestamp without timezone
- **Chat** – id bigserial, name varchar
- **ChatMessage** – id bigserial, chat_id bigint, message varchar, reply_for bigint, author_id bigint, created_at ts wo tz, edited_at ts wo tz
- **ChatMember** – chat_id bigint, user_id bigint, is_owner boolean
- **LastReadMsgInChat** – chat_id, user_id, chat_message_id
- **Node** – id varchar, settings jsonb, last_update ts wo tz

---

## SQL-requests for DB

### `User`

```sql
CREATE TABLE IF NOT EXISTS "User" (
    id BIGSERIAL PRIMARY KEY,
    login VARCHAR NOT NULL,
    pass VARCHAR NOT NULL
);
```

### `Session`

```sql
CREATE TABLE IF NOT EXISTS "Session" (
    id VARCHAR PRIMARY KEY,
    user_id BIGINT NOT NULL REFERENCES "User"(id) ON DELETE CASCADE,
    active_to TIMESTAMP WITHOUT TIME ZONE NOT NULL
);
```

### `Chat`

```sql
CREATE TABLE IF NOT EXISTS "Chat" (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR NOT NULL
);
```
### `ChatMessage`

```sql
CREATE TABLE IF NOT EXISTS "ChatMessage" (
    id BIGSERIAL PRIMARY KEY,
    chat_id BIGINT NOT NULL REFERENCES "Chat"(id) ON DELETE CASCADE,
    message VARCHAR NOT NULL,
    reply_for BIGINT REFERENCES "ChatMessage"(id),
    author_id BIGINT NOT NULL REFERENCES "User"(id) ON DELETE CASCADE,
    created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW(),
    edited_at TIMESTAMP WITHOUT TIME ZONE
);
```
### `ChatMember`

```sql
CREATE TABLE IF NOT EXISTS "ChatMember" (
    chat_id BIGINT NOT NULL REFERENCES "Chat"(id) ON DELETE CASCADE,
    user_id BIGINT NOT NULL REFERENCES "User"(id) ON DELETE CASCADE,
    is_owner BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (chat_id, user_id)
);


```
### `LastReadMsgInChat`

```sql
CREATE TABLE IF NOT EXISTS "LastReadMsgInChat" (
    chat_id BIGINT NOT NULL REFERENCES "Chat"(id) ON DELETE CASCADE,
    user_id BIGINT NOT NULL REFERENCES "User"(id) ON DELETE CASCADE,
    chat_message_id BIGINT NOT NULL REFERENCES "ChatMessage"(id) ON DELETE CASCADE,
    PRIMARY KEY (chat_id, user_id)
);

```

### `Node`

```sql
CREATE TABLE IF NOT EXISTS "Node" (
    id VARCHAR PRIMARY KEY,
    settings JSONB NOT NULL,
    last_update TIMESTAMP WITHOUT TIME ZONE NOT NULL
);
```