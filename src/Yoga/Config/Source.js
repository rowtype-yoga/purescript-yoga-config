const coerceValue = (value) => {
  if (value === "true") return true;
  if (value === "false") return false;
  if (value === "null") return null;
  if (value !== "" && !isNaN(value) && !isNaN(parseFloat(value))) return Number(value);
  return value;
};

export const _getEnvWithPrefix = (prefix) => {
  const result = {};
  const prefixUpper = prefix.toUpperCase() + "_";
  for (const [key, value] of Object.entries(process.env)) {
    if (key.startsWith(prefixUpper)) {
      const path = key.slice(prefixUpper.length).toLowerCase().split("_");
      let current = result;
      for (let i = 0; i < path.length - 1; i++) {
        if (!(path[i] in current) || typeof current[path[i]] !== "object") {
          current[path[i]] = {};
        }
        current = current[path[i]];
      }
      current[path[path.length - 1]] = coerceValue(value);
    }
  }
  return result;
};

export const _resolveEnvRefs = (obj) => {
  const resolve = (node) => {
    if (node === null || node === undefined || typeof node !== "object") return node;
    if (Array.isArray(node)) return node.map(resolve);
    if ("env" in node && typeof node.env === "string") {
      const keys = Object.keys(node);
      const isEnvRef = keys.every(k => k === "env" || k === "default");
      if (isEnvRef) {
        const val = process.env[node.env];
        if (val !== undefined) return val;
        if ("default" in node) return node.default;
        throw new Error(`Environment variable "${node.env}" is not set and no default was provided`);
      }
    }
    const result = {};
    for (const [key, value] of Object.entries(node)) {
      result[key] = resolve(value);
    }
    return result;
  };
  return resolve(obj);
};

export const _deepMerge = (left) => (right) => {
  if (
    typeof left !== "object" || left === null ||
    typeof right !== "object" || right === null ||
    Array.isArray(left) || Array.isArray(right)
  ) {
    return right;
  }
  const result = { ...left };
  for (const key of Object.keys(right)) {
    if (
      key in result &&
      typeof result[key] === "object" && result[key] !== null && !Array.isArray(result[key]) &&
      typeof right[key] === "object" && right[key] !== null && !Array.isArray(right[key])
    ) {
      result[key] = _deepMerge(result[key])(right[key]);
    } else {
      result[key] = right[key];
    }
  }
  return result;
};
