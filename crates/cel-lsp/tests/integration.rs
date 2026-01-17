use cel_lsp::create_service;

#[test]
fn service_can_be_created() {
    let (_service, _socket) = create_service();
}
