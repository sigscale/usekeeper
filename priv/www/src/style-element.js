const styleElement = document.createElement('dom-module');
styleElement.setAttribute('theme-for', 'vaadin-grid');

styleElement.innerHTML = `<template>
		<style>
			:host {
				@apply(--paper-font-common-base);
				--app-primary-color: #f57f17;
				--app-secondary-color: #aeea00;
				display: block;
			}
			dt {
				float: left;
				clear: left;
				width: 24ch;
				text-align: right;
				font-weight: bold;
			}
			dt::after {
				content: ":";
			}
			dd {
				margin: 0 0 0 28ch;
			}
			app-header {
				position: fixed;
				top: 0;
				left: 0;
				width: 100%;
				text-align: center;
				background-color: var(--app-primary-color);
				border-bottom: 1px solid #eee;
				color: #fff;
			}
			.toolbar-top {
				background-color: var(--app-primary-color);
			}
			app-header paper-icon-button {
				--paper-icon-button-ink-color: white;
			}
			app-drawer {
				--app-drawer-content-container: {
					padding-top: 10px;
				};
				height: 100%;
				top: 64px;
			}
			paper-progress {
				display: block;
				width: 100%;
				margin: 0px;
				--paper-progress-active-color: var(--paper-lime-a700);
				--paper-progress-container-color: transparent;
			}
			.drawer-list {
				box-sizing: border-box;
				width: 100%;
				height: 100%;
				background: white;
				position: relative;
			}
			.drawer-list a {
				display: block;
				text-decoration: none;
				color: black;
				padding-left: 24px;
			}
			.drawer-list a.iron-selected {
				color: #78909C;
				font-weight: bold;
			}
			.drawer-list iron-collapse#logs {
				padding-left: 36px;
			}
			.submit-button {
				background-color: var(--paper-lime-a700);
				color: black;
				float: right;
				width: 8em;
			}
			.cancel-button {
				color: black;
			}
         .update-button {
            background-color: var(--paper-lime-a700);
            color: black;
         }
         .dialog {
            overflow: auto;
         }
         .help {
            position: fixed;
            min-width: 20em;
            right: -36px;
            top: 41px;
            overflow: auto;
            display: inline-grid;
         }
			paper-dialog app-toolbar {
				color: white;
				background-color: #bc5100;
			}
         paper-dialog > *:first-child {
            margin-top: 0px;
         }
         paper-dialog iron-collapse {
            --paper-input-container-underline: {
               display: none;
            };
         }
         paper-dialog iron-collapse > div hr {
            border-top: 1px solid blue;
         }
         paper-dialog iron-collapse > div:first-child hr {
            display: none;
         }
         paper-dialog iron-collapse > div {
            padding-top: 25px;
         }
         paper-dialog iron-collapse > div:first-child {
            padding-top: 0px;
         }
			iron-icon {
				padding-right: 10px;
			}
			paper-dropdown-menu {
				--paper-input-container-label: {
					font-size: 24px;
				};
				--paper-input-container-input: {
					font-size: 24px;
					font-weight: 400;
				};
			}
			.grouptitle {
				text-align: center;
				border-bottom-style: solid;
				border-color: var(--paper-yellow-900);
			}
			vaadin-grid {
				height: 100vh;
				font-size: inherit;
			}
			vaadin-grid input {
				font-size: initial;
				border-style: none;
				background: #ffb04c;
				max-width: 130px;
			}
			vaadin-grid input::placeholder {
				color: black;
				font-weight: bold;
				font-size: inherit;
			}
			[part~="header-cell"] {
				background-color: #ffb04c;
			}
			paper-fab {
				background: var(--paper-lime-a700);
				color: black;
			}
			.add-button {
				right: 2%;
				position: fixed;
				bottom: 5%;
				z-index: 100;
			}
			.timestamp {
				direction: rtl;
			}
			paper-card {
				margin: 4px;
				vertical-align: top;
			}
			paper-icon-item {
				--paper-item-icon-width: 32px;
				--paper-item-min-height: 1em;
			}
		</style>
	</template>`;

styleElement.register('style-element');

