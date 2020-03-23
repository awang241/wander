import { shallowMount } from '@vue/test-utils'
import List from "../../src/components/List";

// Mount the component
const wrapper = shallowMount(List, {
    propsData: {
        chosenItems: ["Beans", "Greens", "Tomatoes"],
    }
})


describe('List.vue', () => {
    it('has a created hook', () => {
        expect(true).toBe(true)
    })
    it('Renders the component', () => {
        expect(wrapper.find(List).exists()).toBe(true)
    })
    it('Displays the chosen items', () => {
        expect(wrapper.html()).toContain('Beans')
        expect(wrapper.html()).toContain('Greens')
        expect(wrapper.html()).toContain('Tomatoes')
    })
});