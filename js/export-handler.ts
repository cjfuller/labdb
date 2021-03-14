export default function exporter(getState: Function, format: string) {
  return () => {
    const item = getState();
    const exportURL = `${item.resourcePath}/export?exportformat=${format}`;
    window.location.href = exportURL;
  };
}
