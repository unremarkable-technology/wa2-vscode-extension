import * as path from 'path';
import * as vscode from 'vscode';
import * as fs from 'fs';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
	// Try bundled binary first, fall back to PATH
	const binaryName = process.platform === 'win32' ? 'wa2lsp.exe' : 'wa2lsp';
	const bundledBinary = path.join(context.extensionPath, 'bin', binaryName);

	let serverCommand: string;

	if (fs.existsSync(bundledBinary)) {
		serverCommand = bundledBinary;
		console.log(`WA2: Using bundled LSP server at ${bundledBinary}`);
	} else {
		// Fall back to PATH (for development)
		serverCommand = binaryName;
		console.log(`WA2: Using ${binaryName} from PATH`);
	}

	const serverOptions: ServerOptions = {
		command: serverCommand,
		args: ['--serve'],
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [
			{ language: 'cloudformation-yaml', scheme: 'file' },
			{ language: 'cloudformation-json', scheme: 'file' },
		],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{yml,yaml,json,template,cfn}'),
		},
	};

	client = new LanguageClient(
		'wa2lsp',
		'WA2 LSP',
		serverOptions,
		clientOptions
	);

	// Start the client
	client.start().then(() => {
		console.log('WA2: Language server started successfully');
	}).catch(err => {
		vscode.window.showErrorMessage(`WA2: Failed to start language server: ${err.message}`);
		console.error('WA2: Language server error:', err);
	});

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