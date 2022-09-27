/**
 * Minifies a Buffer containing UTF-8 JavaScript code.
 *
 * @param src - Source JS code
 * @returns Minified JS code
 */
export function minify(topLevelType: "global" | "module", src: Buffer): Buffer;
