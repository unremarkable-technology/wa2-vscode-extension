import * as path from 'path';
import * as vscode from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
	// For now assume 'stackgraph-lsp' is on PATH.
	// Later you can make this configurable.
	const serverCommand = 'stackgraph-lsp';

	const serverOptions: ServerOptions = {
		command: serverCommand,
		args: [],
	};

	const clientOptions: LanguageClientOptions = {
		// We start the server for YAML and JSON files
		documentSelector: [
			{ language: 'yaml', scheme: 'file' },
			{ language: 'json', scheme: 'file' },
		],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{yml,yaml,json}'),
		},
	};

	client = new LanguageClient(
		'wa2lsp',
		'WA2 LSP',
		serverOptions,
		clientOptions
	);

	// Start the client, but do NOT push the promise to subscriptions
	client.start();

	// Properly stop the client on extension deactivation
	context.subscriptions.push({
		dispose: () => client?.stop()
	});
}

export async function deactivate(): Promise<void> {
	if (!client) {
		return;
	}
	await client.stop();
}

