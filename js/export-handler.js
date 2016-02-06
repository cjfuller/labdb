
function exporter(getState, format) {
    return () => {
        const item = getState();
        const exportURL = `${item.resourcePath}/export?exportformat=${format}`;
        window.location.href = exportURL;
    };
}

module.exports = exporter;
