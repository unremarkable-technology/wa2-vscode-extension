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
		'stackgraphLsp',
		'StackGraph LSP',
		serverOptions,
		clientOptions
	);

	client = new LanguageClient(
		'stackgraphLsp',
		'StackGraph LSP',
		serverOptions,
		clientOptions
	);

	// Start the client, but do NOT push the promise to subscriptions
	client.start();

	const disposable = vscode.commands.registerCommand(
		'wa2.helloWorld',
		() => {
			vscode.window.showInformationMessage('WA2: hello from WA2');
		}
	);
	context.subscriptions.push(disposable);

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

