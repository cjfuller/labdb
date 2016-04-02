const SCOPES = ['admin', 'write', 'read'];

/**
 * Helper for showing auth-appropriate actions in the UI.
 *
 * This ensures that we won't show buttons for actions that would be rejected
 * due to lack of privileges on the server.
 *
 * (Note that this should never act as the primary means of securing access
 * based on authorization scope.)
 */
function hasAuthScope(scope) {
    if (!SCOPES.includes(scope)) {
        return false;
    }
    const providedAuthScope = window._labdbAuth;
    if (providedAuthScope === 'admin') {
        // admin implies access to any other scope
        return true;
    }
    if (providedAuthScope === 'write' && (
        scope === 'read' || scope === 'write')) {
        return true;
    }
    if (providedAuthScope === scope) {
        return true;
    }
    return false;
}

module.exports = hasAuthScope;
