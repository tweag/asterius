export async function getNodeModules() {
  try {
    return {
      fs: (await import("fs")).default
    };
  } catch (err) {
    return undefined;
  }
}
