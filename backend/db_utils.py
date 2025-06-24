# This file is no longer needed - backend now uses direct psql commands

def get_conversations():
    """Fetch all conversations from database"""
    query = """
        SELECT id, title, created_at, updated_at 
        FROM conversations 
        WHERE user_id = 1 
        ORDER BY updated_at DESC
    """
    
    try:
        result = execute_sql_via_api(query)
        conversations = []
        
        if isinstance(result, list):
            for row in result:
                conversations.append({
                    "id": row.get("id"),
                    "title": row.get("title"),
                    "createdAt": row.get("created_at") + "Z" if row.get("created_at") else None,
                    "updatedAt": row.get("updated_at") + "Z" if row.get("updated_at") else None
                })
        
        return conversations
    except Exception as e:
        logger.error(f"Error fetching conversations: {e}")
        return []

def create_conversation(title="New Conversation"):
    """Create a new conversation in database"""
    import uuid
    
    conv_id = str(uuid.uuid4())
    now = datetime.utcnow().isoformat()
    
    query = f"""
        INSERT INTO conversations (id, user_id, title, created_at, updated_at)
        VALUES ('{conv_id}', 1, '{title}', '{now}', '{now}')
        RETURNING id, title, created_at, updated_at
    """
    
    try:
        result = execute_sql(query)
        if isinstance(result, list) and len(result) > 0:
            row = result[0]
            return {
                "id": row.get("id"),
                "title": row.get("title"),
                "createdAt": row.get("created_at") + "Z" if row.get("created_at") else None,
                "updatedAt": row.get("updated_at") + "Z" if row.get("updated_at") else None
            }
        return None
    except Exception as e:
        logger.error(f"Error creating conversation: {e}")
        return None

def get_conversation_with_messages(conversation_id):
    """Get conversation with all messages"""
    # First get conversation
    conv_query = f"""
        SELECT id, title, created_at, updated_at 
        FROM conversations 
        WHERE id = '{conversation_id}' AND user_id = 1
    """
    
    # Then get messages
    msg_query = f"""
        SELECT id, role, content, displays, timestamp
        FROM messages 
        WHERE conversation_id = '{conversation_id}' 
        ORDER BY timestamp ASC
    """
    
    try:
        conv_result = execute_sql(conv_query)
        if not conv_result or len(conv_result) == 0:
            return None
            
        conv_row = conv_result[0]
        
        # Get messages
        msg_result = execute_sql(msg_query)
        messages = []
        
        if isinstance(msg_result, list):
            for msg_row in msg_result:
                messages.append({
                    "id": msg_row.get("id"),
                    "role": msg_row.get("role"),
                    "content": msg_row.get("content"),
                    "displays": msg_row.get("displays") or [],
                    "timestamp": msg_row.get("timestamp") + "Z" if msg_row.get("timestamp") else None
                })
        
        return {
            "id": conv_row.get("id"),
            "title": conv_row.get("title"),
            "createdAt": conv_row.get("created_at") + "Z" if conv_row.get("created_at") else None,
            "updatedAt": conv_row.get("updated_at") + "Z" if conv_row.get("updated_at") else None,
            "messages": messages
        }
        
    except Exception as e:
        logger.error(f"Error fetching conversation {conversation_id}: {e}")
        return None

def save_message(conversation_id, role, content, displays=None):
    """Save a message to the database"""
    import uuid
    
    msg_id = str(uuid.uuid4())
    now = datetime.utcnow().isoformat()
    displays_json = json.dumps(displays or [])
    
    query = f"""
        INSERT INTO messages (id, conversation_id, role, content, displays, timestamp)
        VALUES ('{msg_id}', '{conversation_id}', '{role}', '{content.replace("'", "''")}', '{displays_json}', '{now}')
    """
    
    try:
        execute_sql(query)
        return {
            "id": msg_id,
            "role": role,
            "content": content,
            "displays": displays or [],
            "timestamp": now + "Z"
        }
    except Exception as e:
        logger.error(f"Error saving message: {e}")
        return None

def update_conversation_title(conversation_id, title):
    """Update conversation title"""
    now = datetime.utcnow().isoformat()
    
    query = f"""
        UPDATE conversations 
        SET title = '{title.replace("'", "''")}', updated_at = '{now}' 
        WHERE id = '{conversation_id}' AND user_id = 1
    """
    
    try:
        execute_sql(query)
        return True
    except Exception as e:
        logger.error(f"Error updating conversation title: {e}")
        return False