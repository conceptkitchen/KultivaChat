# Kultivate AI - Secure Migration Guide

## Current System Credential Locations

**Replit Environment Variables** (accessible via Replit Secrets):
```bash
# Check current values with:
echo $DATABASE_URL
echo $KBC_STORAGE_TOKEN  
echo $GEMINI_API_KEY
echo $SESSION_SECRET
echo $GOOGLE_PROJECT_ID
echo $KBC_WORKSPACE_ID
```

**File-Based Credentials**:
- Google Service Account JSON: `/home/runner/workspace/backend/credentials-839-21894808.json`
- Check if file exists: `ls -la backend/credentials-*.json`

## Secure Migration Steps

### 1. Export Current Environment Variables
Run this in your current Replit environment:
```bash
# Create secure backup of environment variables
echo "DATABASE_URL=$DATABASE_URL" > migration_env_backup.txt
echo "KBC_STORAGE_TOKEN=$KBC_STORAGE_TOKEN" >> migration_env_backup.txt
echo "GEMINI_API_KEY=$GEMINI_API_KEY" >> migration_env_backup.txt
echo "SESSION_SECRET=$SESSION_SECRET" >> migration_env_backup.txt
echo "GOOGLE_PROJECT_ID=$GOOGLE_PROJECT_ID" >> migration_env_backup.txt
echo "KBC_WORKSPACE_ID=$KBC_WORKSPACE_ID" >> migration_env_backup.txt
```

### 2. Extract Service Account JSON
```bash
# Copy service account file to accessible location
cp backend/credentials-839-21894808.json migration_service_account.json
# Verify file contents
head -5 migration_service_account.json
```

### 3. Database Credentials Extraction
```bash
# Extract database connection details from DATABASE_URL
echo $DATABASE_URL | sed 's/postgresql:\/\/\([^:]*\):\([^@]*\)@\([^:]*\):\([^\/]*\)\/\(.*\)/Host: \3\nPort: \4\nDatabase: \5\nUsername: \1\nPassword: \2/'
```

### 4. Verify Current System Access
Test all credentials are working:
```bash
# Test database connection
curl -s "http://localhost:5000/api/conversations" | head -20

# Test backend health
curl -s "http://localhost:8081/api/health" 

# Check conversation count
curl -s "http://localhost:5000/api/conversations" | jq length
```

## Migration Checklist

- [ ] Current system fully functional (test chat interface)
- [ ] All environment variables backed up
- [ ] Service account JSON file copied
- [ ] Database connection tested
- [ ] Keboola API token verified
- [ ] Gemini API key confirmed working
- [ ] New environment prepared
- [ ] Credentials transferred via secure channel
- [ ] New system tested
- [ ] Old credentials rotated/revoked

## Secure Transfer Methods

1. **Encrypted File Transfer**: Use tools like scp with SSH keys
2. **Secure Messaging**: Use encrypted messaging platforms  
3. **Cloud Secret Management**: Use Google Secret Manager or AWS Secrets Manager
4. **Direct Console Access**: Manually copy through secure admin interfaces

## Post-Migration Security

1. **Rotate All Credentials**: Generate new tokens after migration
2. **Update Access Logs**: Monitor for any unauthorized usage
3. **Test New Environment**: Verify all functionality works
4. **Cleanup**: Delete migration backup files securely

## Current System Status Check

Let me verify what credentials are currently active in your system...