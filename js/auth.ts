export type Scope = "admin" | "write" | "read";

/**
 * Helper for showing auth-appropriate actions in the UI.
 *
 * This ensures that we won't show buttons for actions that would be rejected
 * due to lack of privileges on the server.
 *
 * (Note that this should never act as the primary means of securing access
 * based on authorization scope.)
 */
export default function hasAuthScope(scope: Scope) {
  const providedAuthScope: Scope | null | undefined = (window as any)
    ._labdbAuth;
  if (providedAuthScope === "admin") {
    // admin implies access to any other scope
    return true;
  }
  if (
    providedAuthScope === "write" &&
    (scope === "read" || scope === "write")
  ) {
    return true;
  }
  if (providedAuthScope === scope) {
    return true;
  }
  return false;
}
