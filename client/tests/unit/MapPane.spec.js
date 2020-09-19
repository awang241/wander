import {shallowMount} from '@vue/test-utils'
import MapPane from "../../src/components/Location/MapPane";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(MapPane, {
        mocks: {
            google: jest.fn()
        }
    });
})

afterEach(() => {
    wrapper.destroy();
});

describe('MapPane.vue', () => {
    test('Restore label on button is shown when map is not minimized', () => {
        wrapper.vm.width = 500
        wrapper.vm.height = 500
        let button = wrapper.find("#resizeButton")
        expect(button.text()).toBe("Minimize Map");
    });
    test('Ensure computed function returns false if map is not minimized', () => {
        wrapper.vm.width = 200
        wrapper.vm.height = 783
        expect(wrapper.vm.isMinimized).toBeFalsy();
    });

    test('Ensure computed function returns true if map is minimized', () => {
        wrapper.vm.width = 0
        wrapper.vm.height = 0
        expect(wrapper.vm.isMinimized).toBeTruthy();
    });
})