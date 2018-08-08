\connect jabber;

-- The additional table for custom chat markers
CREATE TABLE IF NOT EXISTS read_messages (
  user_jid TEXT NOT NULL,
  room_jid TEXT NOT NULL,
  last_message_id BIGINT NOT NULL,
  last_message_at BIGINT NOT NULL,
  unseen_messages INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT now(),
  PRIMARY KEY (user_jid, room_jid)
);
